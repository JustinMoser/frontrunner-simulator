>  || literate script    
 
This file sets out an experiment to explore the behaviour of front-running HFTs,
based on the description given by Michael Lewis in his book "Flash Boys"

The experiment contains the following:

- a populator trader, whose only job is to send limit orders (Good Until Cancelled) to the two exchanges; 
  this is done right at the start of the simulation (in the first ten time steps) and then the populator 
  trader simply waits for trade confirmations.  To simplify the experiment, we assume that front-running 
  is done against an order to buy and so the populator only issues asks to provide liquidity at the two
  exchanges on one side of the book only.

- two exchanges

- a broker trader, who places orders on behalf of a client.  The client is not modelled in the experiment,
  we just require that the broker must execute a Buy order for 100,000 shares and must achieve best 
  execution for the client.  This might be repeated several times, so that we can explore whether
  a frontrunner can achieve consistent profits.
        1.  set Z = 100,000
        2.  the broker now has to execute a Buy order of size Z
        3.  the broker checks the askbooks at both exchanges
        4.  the broker determines which exchange has the best price, and what size ("X") Buy order can be
            filled at that exchange before its price becomes worse than that at the other exchange.
        5.  set x = min(X,Z)
        6.  if (x<Z), the broker also calculates what size ("Y") order can be filled at the second 
            exchange before its price becomes worse than that at the first exchange (assuming the 
            Buy order for "X" shares had been fully executed at the first exchange)
        7.  set y = min(Y,Z-x)
        8.  the broker sends a Buy to the first exchange for size x and a Buy order to the second 
            exchange for size y
        9.  set Z=Z-(x+y)
        10. repeat from 2
     

- an HFT frontrunner trader, who monitors the two exchanges and tries to gain consistent profit at
  low or no risk, simply by virtue of faster communications.  This is a very simple frontrunner, we
  can introduce more sophistication later
        1.  check the askbooks on the two exchanges
        2.  calculate sizes x and y as explained above (broker trader algorithm) - however, the
            frontrunner doesnt know the brokers total trade size Z.  Therefore, the frontrunner
            guesses that Z=1,000.  Call the HFTs version of x and y "xx" and "yy"
        3.  check whether trades for total size xx have been executed in one timestep on the exchange 
            with the best price
        4.  if so, send two orders to the second exchange: (i) a Buy for "yy", and (ii) an Ask for
            "yy" at the best ask price (after Buying has succeeded)-1.  [optional - work out in 
            advance whether this will bring sufficient profit, and if not dont bother!]
        5.  receive trade confirmations - if all trades executed, repeat from 1.  Otherwise, take
            emergency action (if left with positive inventory, issue an Ask at best bid + 1, and
            vice versa

- delays:  - The populator can send and receive messages from both exchanges in one time step 
             (essentially we dont care about delays for this trader)
           - The broker can send and receive messages to and from exchange 1 in two time steps 
             (delay = 1) and can send and receive messages to and from exchange 2 in 20 time steps
             (delay = 19).
           - The HFT can send and receive messages to and fom both exchanges in one time step
             (no delays to either exchange)

===================
EXPERIMENT SETTINGS
===================

>allfillandkill = False

==========================
TYPES AND HELPER FUNCTIONS
==========================
 
>order_t ::= Order ordertype_t ordersize_t orderprice_t orderid_t ordertime_t exchid_t
>ordertype_t ::= Bid | Ask | Buy | Sell 
>ordersize_t == num 
>orderprice_t == num 
>orderid_t == num 
>ordertime_t == num 
>exchid_t == num
 
And here are some helper functions: 
 
>getorderid         (Order ty sz p i t exchid)       = i 
>getordersize       (Order ty sz p i t exchid)       = sz 
>getorderprice      (Order ty sz p i t exchid)       = p 
>getordertime       (Order ty sz p i t exchid)       = t 
>setordertime       (Order ty sz p i t exchid) newt  = Order ty sz p i newt exchid
>setordersize       (Order ty sz p i t exchid) newsz = Order ty newsz p i t exchid
>setorderprice      (Order ty sz p i t exchid) newp  = Order ty sz newp i t exchid
>subtractordersize  (Order ty sz p i t exchid) subsz = Order ty (sz-subsz) p i t exchid
>getorderexchid     (Order ty sz p i t exchid)       = exchid
>setorderexchid     (Order ty sz p i t exchid) newexchid = Order ty sz p i t newexchid

>selectmin f xs = foldl g (hd xs) xs
>                 where
>                 g a b = a, if(f a < f b)
>                       = b, otherwise

>volumeatexch::[order_t] -> [order_t] -> num -> num
>volumeatexch (x:xs) (y:ys) vol = vol + (getordersize x) + (volumeatexch xs (y:ys) vol), if ((getorderprice x) < (getorderprice y))
>                               = vol, otherwise

Here is a function to decide where to place Buy orders given two askbooks and a target order size
It takes args (i) askbook (ii) askbook (iii) ordersize for ex1 (iv) ordersize for ex2 (v) targetordersize

>decideorders :: [order_t]  -> [order_t] -> num -> num -> num -> [(num, num)]
>decideorders as1      as2      s1 s2 0   = [(1,s1), (2,s2)]
>decideorders []       as2      0  s2 inv = [(2,s2+inv)]
>decideorders []       as2      s1 s2 inv = [(1,s1), (2,s2+inv)]
>decideorders as1      []       s1 0  inv = [(1,s1+inv)]
>decideorders as1      []       s1 s2 inv = [(1,s1+inv), (2,s2)]
>decideorders (a1:as1) (a2:as2) s1 s2 inv = decideorders as1      (a2:as2) (s1+ss1) s2       (inv-ss1), if (p1<p2)||((p1=p2)&(ss1>=ss2))
>                                         = decideorders (a1:as1) as2      s1       (s2+ss2) (inv-ss2), otherwise
>                                           where
>                                           p1  = getorderprice a1
>                                           p2  = getorderprice a2
>                                           ss1 = getordersize a1
>                                           ss2 = getordersize a2


Get order size from 1 item in the list result from decide orders

>os :: (num,num) -> num
>os (exchid,ordersize) = ordersize

Get order exchange id from 1 item in the list result from decide orders 

>oex :: (num,num) -> num
>oex (exchid,ordersize) = exchid


====================
BROKER AGENT DETAILS
====================

The broker has a fixed size order, from a client that needs to be executed as market orders only. 
i.e that it will try to fill the order in its entirety at current best price, in blocks, across both exchanges. 
When one best ask has been filled, it will move onto the next best ask until the entire order has been filled.
 
>broker xq (((bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1):rest1),
>            ((bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2):rest2)) oldinv starttime mosize time id
>              = [(bid1,ask1,sell1,buy1,newinv),(bid2,ask2,sell2,buy2,newinv)] : (broker xq (rest1,rest2) newinv starttime mosize (time+1) id)
>                where
>                bid1 = Order Bid 0 0 id time 0
>                bid2 = Order Bid 0 0 id time 1
>                ask1 = Order Ask 0 0 id time 0
>                ask2 = Order Ask 0 0 id time 1
>                buy1 = Order Buy (buysize1 (decideorders asks1 asks2 0 0 mosize)) 0 id time 0, if((oldinv < mosize)&(time>=starttime))
>                     = Order Buy 0 0 id time 0, otherwise
>                buy2 = Order Buy (buysize2 (decideorders asks1 asks2 0 0 mosize)) 0 id time 1, if((oldinv < mosize)&(time>=starttime))
>                buy2 = Order Buy 0 0 id time 1, otherwise
>                sell1 = Order Sell 0 0 id time 0 
>                sell2 = Order Sell 0 0 id time 1
>                buysize1 (x:xs) = os x
>                buysize2 (x:xs) = os (hd xs) 
>      		     newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))



=======================
POPULATOR AGENT DETAILS
=======================

The populator agent issues ask orders at various prices for 10 timesteps, 
up until the broker agent begins issuing buy orders. This creates liquidity
for this experinent


>populator xq (((bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1):rest1),
>              ((bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2):rest2)) oldinv stoptime time id || Make sure stoptime = startime-1
>                = [(bid, ask, sell, buy, newinv)] : (populator xq (rest1,rest2) newinv stoptime (time+1) id)
>                  where
>                  bid = Order Bid 0 0 id time exchid
>                  ask = Order Ask (populator_sizes!time) (populator_prices!time) time id exchid, if (time < stoptime)
>                      = Order Ask 0 0 id time exchid, otherwise
>                  buy = Order Buy 0 0 id time exchid
>                  sell = Order Sell 0 0 id time exchid
>                  exchid = 0, if ((time mod 2) = 0)
>                         = 1, otherwise
>                  newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                  psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>

==========================
FRONT RUNNER AGENT DETAILS
==========================


The frontrunner agent monitors both exchanges, and issues very small (often 1 share) ask orders at the best price, in order to
determine their counterparty. Once that has been done, they look at the historical average order size for the counterparty
and look to pre empt their order at best ask price across exchanges, thus giving them the ability to then sell the order back
to the counterparty at a higher price. This is known as front-running.


>frontrunner xq (((bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1):rest1),
>                ((bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2):rest2)) oldinv estsize time id
>                  = [(bid, ask, sell, buy, newinv)] : (frontrunner xq (rest1,rest2) oldinv estsize (time+1) id)
>                    where
>                    order1size = os (hd (decideorders asks1 asks2 0 0 estsize))
>                    order2size = os (hd (tl (decideorders asks1 asks2 0 0 estsize)))
>                    bid = Order Bid 0 0 id time 0
>                    buy = Order Buy (estsize - order1size) 0 id time 1, if(order1size=(getordersize (hd xbuys1)))
>                    ask = Order Ask (estsize - order1size) ((newbestask asks2 (estsize - order1size))-1) id time 1
>                    sell = Order Sell 0 0 id time 0
>                    newbestask (x:xs) buysize = (getorderprice (x)), if ((getordersize x)>buysize)
>                                              = newbestask xs (buysize - (getordersize x)), otherwise
>                    newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                    psi i os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))


The exchange takes in lists of orders from the traders and produces the bestbid and bestask plus 
lists of confirmations (and price, inventories, sellp and buyp for the graphs).   
The exchange also takes in and outputs its own bidbook and askbook.  The exchange has id 0. 
 
>exch:: num -> [([order_t],[order_t],[order_t],[order_t],[num])]->num->[order_t]->[order_t] -> num 
>           -> [([order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],num,num,num,[num],num,num,[num])] 
>exch id ((allbids,allasks,allsells,allbuys,invs):rest) time bbook abook ltp 
>    = (bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bb, ba, newltp, pp, sellp, buyp, invs) : (exch id rest (time+1) newbbook newabook newltp) 
>      where 
>      bids = (filter ((=id).getorderexchid) allbids)
>      asks = (filter ((=id).getorderexchid) allasks)
>      sells = (filter ((=id).getorderexchid) allsells)
>      buys = (filter ((=id).getorderexchid) allbuys)
>      bbook1 = insb bids [], if allfillandkill 
>             = insb bids bbook, otherwise 
>               where 
>               insb []     []     = [] 
>               insb []     ys     = ys 
>               insb (x:xs) []     = insb xs [], if (getordersize x) = 0 
>                                  = insb xs [x], otherwise 
>               insb (x:xs) (y:ys) = insb xs (y:ys), if (getordersize x) = 0 
>                                  = insb xs (x:(y:ys)), if (getorderprice x) > (getorderprice y) 
>                                  = y:(insb (x:xs) ys), otherwise 
>      abook1 = insa asks [], if allfillandkill 
>             = insa asks abook, otherwise 
>               where 
>               insa []     []     = [] 
>               insa []     ys     = ys 
>               insa (x:xs) []     = insa xs [], if (getordersize x) = 0 
>                                  = insa xs [x], otherwise 
>               insa (x:xs) (y:ys) = insa xs (y:ys), if (getordersize x) = 0 
>                                  = insa xs (x:(y:ys)), if (getorderprice x) < (getorderprice y) 
>                                  = y:(insa (x:xs) ys), otherwise 
>      (xbids1, xasks1, bbook2, abook2)  
>             = ([],[],bbook1,abook1) 
>             ||= uncross bbook1 abook1 
>             ||  where 
>             ||  uncross [] [] = ([],[],[],[]) 
>             ||  uncross [] ys = ([],[],[],ys) 
>             ||  uncross xs [] = ([],[],xs,[]) 
>             ||  uncross (x:xs) (y:ys) = (xb:bs, xa:as, nbb, nab), if (getorderprice x) >= (getorderprice y) 
>             ||                        = ([],[],(x:xs),(y:ys)), otherwise 
>             ||                          where 
>             ||                          (bs, as, nbb, nab) = uncross xs1 ys1 
>             ||                          exsize  = min [getordersize x, getordersize y] 
>             ||                          exprice = getorderprice x, if (getordertime x <= getordertime y) 
>             ||                                  = getorderprice y, otherwise 
>             ||                          xb = setordersize (setorderprice x exprice) exsize 
>             ||                          xa = setordersize (setorderprice y exprice) exsize 
>             ||                          xs1 = xs, if (getordersize y) >= (getordersize x) 
>             ||                              = runt:xs, otherwise 
>             ||                                where 
>             ||                                runt = subtractordersize x ((getordersize x) - exsize) 
>             ||                          ys1 = ys, if (getordersize x) >= (getordersize y) 
>             ||                              = runt:ys, otherwise 
>             ||                                where 
>             ||                                runt = subtractordersize y ((getordersize y) - exsize) 
>      (xbids2, xsells, newbbook) = match bbook2 sells 
>      (xasks2, xbuys,  newabook) = match abook2 buys 
>      (xbids, xasks) = (xbids1++xbids2, xasks1++xasks2) 
>      newltp = min (map getorderprice xbids), if xbids ~= [] 
>             = max (map getorderprice xasks), if xasks ~= [] 
>             = ltp, otherwise 
>      sellp = (foldr (+) 0 (map getordersize sells)) /(1+ (foldr (+) 0 (map getordersize bbook1))) 
>      buyp = (foldr (+) 0 (map getordersize buys)) / (1+ (foldr (+) 0 (map getordersize abook1))) 
>      bb = 0, if (newbbook = []) 
>         = getorderprice (hd newbbook), otherwise 
>      ba = 0, if (newabook = []) 
>         = getorderprice (hd newabook), otherwise 
>      pp = interleave xbuys xsells || assume interleaved selling and buying 
>           where 
>           interleave []     ys     = map getorderprice ys 
>           interleave xs     []     = map getorderprice xs 
>           interleave (x:xs) (y:ys) = (getorderprice x):((getorderprice y):(interleave xs ys)) 
>      match ls     []     = ([],[],ls)  || xlims, xmos, newbook 
>      match []     ms     = ([],[],[]) 
>      match (l:ls) (m:ms) = match (l:ls) ms, if (getordersize m)=0 
>                          = match ls (m:ms), if (getordersize l)=0 
>                          = (l:a1,xmark1:b1,c1),  if (getordersize l) < (getordersize m) 
>                          = (xlim2:a2, m2:b2,c2), if (getordersize l) > (getordersize m) 
>                          = (l:a3, m2:b3,c3), otherwise 
>                            where
>                            (a1,b1,c1) = match ls (m1:ms) 
>                            (a2,b2,c2) = match (l1:ls) ms 
>                            (a3,b3,c3) = match ls ms 
>                            lsize = getordersize l 
>                            lprice = getorderprice l 
>                            msize = getordersize m 
>                            xmark1 = setorderprice (setordersize m lsize) lprice 
>                            m1 = subtractordersize m lsize 
>                            m2 = setorderprice m lprice 
>                            xlim2 = setordersize l msize 
>                            l1 = subtractordersize l msize

We will need a delay component as follows: 
 
>delay n []     = [] 
>delay n (x:xs) = (rep n x)++xs 
 
 
Now we need to run a numerical simulation which uses the above definitions. 
 
>sim steps = f allexchstates steps 
>            where 
>            allexchstates = ((initexchstate : (exch 0 allmessages 1 [] [] startprice)), (initexchstate : (exch 1 allmessages 1 [] [] startprice))) 
>            initexchstate = ([],[],[],[],[],[],[],[],startprice-(startspread/2),startprice+(startspread/2),startprice,[],0,0,[])  
>                            || bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,ba,ltp,pp,sellp,buyp,invs 
>            allmessages    = mergedmessages 
>            tradermessages = map g (zip2 (agents allexchstates) [1..]) 
>                             where 
>                             g (f,id) = f 0 id 
>            mergedmessages = f tradermessages 
>                             where 
>                             f []         = [] 
>                             f ([]:rest)  = []   || if any trader has stopped issuing messages, we halt the simulation 
>                             f any        = (g (map hd any) [([], [], [], [], []), ([], [], [], [], [])]) : (f (map tl any)) 
>                                                         || (bids, asks, sells, buys, invs)  
>                                                         || All collected together for each time step 
>                                                         || NB this does not depend on the number of traders - so it should work fine! 
>                                                         || (I confused simplesim with big sim!) 
>                             g [] [(a,b,c,d,e),(f,g,h,i,j)] = [(a,b,c,d,e),(f,j,h,i,j)]
>                             g ([(bid1,ask1,buy1,sell1,inv1),(bid2,ask2,buy2,sell2,inv2)]:rest) [(bids1,asks1,buys1,sells1,invs1),(bids2,asks2,buys2,sells2,invs2)]
>                                = g rest [(bid1:bids1, ask1:asks1, buy1:buys1, sell1:sells1, invs1++[inv1]),(bid2:bids2, ask2:asks2, buy2:buys2, sell2:sells2, invs2++[inv2])]
>                              || 
>                              ||([(agent1 exch1 output),(agent1 exch2 output)]:others) [(bids1, asks1, buys1, sells1, invs1),(bids2, asks2, buys2, sells2, invs2)] 
>                              || 
 
>f (a,b) steps = (take steps a, take steps b)

And now finally we need to output the sim results to file 
 
>runtest steps = [Tofile "simplesim12mm.csv" (hdrs ++ (g startprice 0 (sim steps))), Closefile "simplesim2mm.csv", System "/Applications/Microsoft \\Office\\ 2011/Microsoft\\ Excel.app/Contents/MacOS/Microsoft\\ Excel &" ] 
>                where 
>                ||hdrs = "Price,SellPressure,BuyPressure,Inv5,Inv4,Inv3,Inv2,Inv1" 
>                hdrs = "Time,Bids,Asks,Buys,Sells,BidPrice1,BidPrice2,AskPrice1,AskPrice2,AskSize1,Asksize2,BuySize1,BuySize2,SellSize1,SellSize2,Price,SellPressure,BuyPressure,Net Pressure,BestBid,BestAsk,Invs\n"
>                g p t ([],[])     = []
>                g p t ([],b)      = []
>                g p t (a,[])      = []
>                g p t (a:aa,b:bb) = output ++ (g ltp (t+1) (aa,bb)) 
>                                    where 
>                                    output = output1 ++ output2
>                                    ltp = ltp2          || chosen arbritrarily
>                                    (output1,ltp1) = h p t a
>                                    (output2,ltp2) = h p t b
>                h p t (bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,ba,ltp,pp,sellp,buyp,invs) 
>                         = k  
>                             (foldr (+) 0 (map getordersize bids))  || Bids
>                             (foldr (+) 0 (map getordersize asks))  || Asks
>                             (foldr (+) 0 (map getordersize buys))  || Buys
>                             (foldr (+) 0 (map getordersize sells))  || Sells
>                             ((getorderprice.safehd) (filter ((=1).getorderid) bids))  || BidPrice1
>                             ((getorderprice.safehd) (filter ((=2).getorderid) bids))  || BidPrice2
>                             ((getorderprice.safehd) (filter ((=1).getorderid) asks))  || AskPrice1
>                             ((getorderprice.safehd) (filter ((=2).getorderid) asks))  || AskPrice2
>                             ((getordersize.safehd) (filter ((=1).getorderid) asks))   || AskSize1
>                             ((getordersize.safehd) (filter ((=2).getorderid) asks))   || AskSize2
>                             ((getordersize.safehd) (filter ((=1).getorderid) buys))   || BuySize1
>                             ((getordersize.safehd) (filter ((=2).getorderid) buys))   || BuySize2
>                             ((getordersize.safehd) (filter ((=1).getorderid) sells))  || SellSize1
>                             ((getordersize.safehd) (filter ((=2).getorderid) sells))  || SellSize2
>                             sellp || SellPressure
>                             buyp  || BuyPressure
>                             bb    || BestBid
>                             ba    || BestAsk
>                             invs  || Inventories
>                             pp    || Prices
>                             ltp   || LastTradePrice
>                           where 
>                           safehd [] = Order Bid 0 0 0 0 
>                           safehd (x:xs) = x 
>                           showwithcommas []     = "" 
>                           showwithcommas [x]    = (shownum x) 
>                           showwithcommas (x:xs) = (shownum x)++","++(showwithcommas xs) 
                         
                            k  =
                                bi = bids
                                as = asks
                                bu = buys
                                se = sells
                                bi1 = bid price 1
                                bi2 = bid price 2
                                as1 = ask price 1
                                as2 = ask price 2
                                asize1 = ask size 1
                                asize2 = ask size 2
                                bu1 = buys size 1
                                bu2 = buy size 2
                                se1 = sell size 1
                                se2 = sell size 2
                                sp = sell pressure
                                bp = buy pressure
                                bb = best bid
                                ba = best ask
                                is = inventories
                                xs = prices
                                ltp = last trade price

>                           k  bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is [] ltp 
>                              = ((showwithcommas ([t,bi,as,bu,se,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,ltp,sp,bp,(sp-bp),bb,ba]++is))++"\n",ltp) 
>
>                           k  bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is xs ltp  
>                              = (kk bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is xs, ltp) 
>
>                           kk bi as bu se bi1 bi1 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is [] = error "kk applied to []" 
>
>                           kk bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is [x]     
>                              = (showwithcommas ([t,bi,as,bu,se,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,x,sp,bp,(sp-bp),bb,ba]++is))++"\n" 
>
>                           kk bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is (x:xs)  
>                              = ((showwithcommas ([t,bi,as,bu,se,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,x,sp,bp,(sp-bp),bb,ba]++is))++"\n") 
>                                ++(kk bi as bu se bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 sp bp bb ba is xs)  
 
 
For each experiment we need to specify some agents as follows. This is a list of partial applications of the function "mm";  
the function "sim" will apply each of these to its start time and to its unique agent id  
(NB I changed the order of the arguments to mm and fs so this would work): 
 
>agents agentinput = [
>                     broker initxq agentinput 0 buyer_start_time broker_mo_size,
>                     populator initxq agentinput 0 (buyer_start_time-1),
>                     frontrunner initxq agentinput 0 broker_mo_size
>                    ] 
>                    where 
>                    initxq = ([],[],[],[]) 
 
 
Other parameters for this experiment: 
 
>ul = 3000 
>ll = (-ul) 
>delta1 = 0 || 2 
>delaytime1 = 90 
>delta2 = 3 || 3 
>delaytime2 = 130 
>delta3 = 6 || 4 
>delaytime3 = 170 
>startprice=1113 
>startspread=20 
>fsellsize=500 ||2500 || 4000 
>fstoptime= 180 
>market_order_size= 177 
>mmsizes = [1650,200,20,-20,40,-40,60,-60,80,-80,100,-100] || [3300,20,-20,40,-40,60,-60,0,0,800,-800,0] 
>buyer_start_time = 10 || 10 
>populator_sizes = [1500,800,1250,900,250,600,1720,500,300,175]
>populator_prices = [55,57,62,68,71,75,52,65,54,66]
>broker_mo_size = 7000
