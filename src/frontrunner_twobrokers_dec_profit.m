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
 
>order_t ::= NoOrder | Order ordertype_t ordersize_t orderprice_t orderid_t ordertime_t exchid_t
>ordertype_t ::= Bid | Ask | Buy | Sell | NoOrderType
>ordersize_t == num 
>orderprice_t == num 
>orderid_t ::= NoId | Id num 
>ordertime_t == num 
>exchid_t == num
 
And here are some helper functions: 
 
>getorderid         (Order ty sz p (Id i) t exchid)  = i 
>getorderid         (Order ty sz p NoId   t exchid)  = error "getorderid: no order Id" 
>getorderid         (NoOrder)                        = error "getorderid: haven't decided yet what to return" 
>getordersize       (Order ty sz p i t exchid)       = sz 
>getordersize       (NoOrder)                        = error "getordersize: haven't decided yet what to return" 
>getorderprice      (Order ty sz p i t exchid)       = p 
>getorderprice      (NoOrder)                        = error "getorderprice: haven't decided yet what to return" 
>getordertype       (Order ty sz p i t exchid)       = ty 
>getordertype       (NoOrder)                        = NoOrderType
>getordertime       (Order ty sz p i t exchid)       = t 
>getordertime       (NoOrder)                        = error "getordertime: haven't decided yet what to return" 
>setordertime       (Order ty sz p i t exchid) newt  = Order ty sz p i newt exchid
>setordertime       (NoOrder) newt                   = error "setordertime: haven't decided yet what to return" 
>setordersize       (Order ty sz p i t exchid) newsz = Order ty newsz p i t exchid
>setordersize       (NoOrder) newsz                  = error "setordersize: haven't decided yet what to return" 
>setorderprice      (Order ty sz p i t exchid) newp  = Order ty sz newp i t exchid
>setorderprice      (NoOrder) newp                   = error "setorderprice: haven't decided yet what to return" 
>subtractordersize  (Order ty sz p i t exchid) subsz = Order ty (sz-subsz) p i t exchid
>subtractordersize  (NoOrder) subsz                  = error "subtractordersize: haven't decided yet what to return" 
>getorderexchid     (Order ty sz p i t exchid)       = exchid
>getorderexchid     (NoOrder)                        = error "getorderexchid: haven't decided yet what to return" 
>setorderexchid     (Order ty sz p i t exchid) newexchid = Order ty sz p i t newexchid
>setorderexchid     (NoOrder) newexchid              = error "setorderexchid: haven't decided yet what to return"


=========================
STRING HELPER FUNCTIONS
=========================

>string == [char]
>int_to_string :: num -> string
>int_to_string n = "-" ++ pos_to_string (abs n), if n < 0
>                = pos_to_string n, otherwise

>pos_to_string :: num -> string
>pos_to_string n = int_to_char n, if n < 10
>                = pos_to_string (n div 10) ++ (int_to_char (n mod 10)), otherwise

>int_to_char :: num -> string
>int_to_char n = show (decode (n + code '0'))


========================
DEBUG HELPER FUNCTIONS
========================

>bookcount :: [order_t] -> num -> num
>bookcount [] count = count
>bookcount (x:xs) count = (bookcount xs (count+1))

>tracebooks :: [order_t] -> [order_t] -> [char] -> (num,num) -> [char] 
>tracebooks asks1 asks2 str (exchid,os) = "AskBook1 #: " ++ int_to_string (bookcount asks1 0) ++ " " ++ "AskBook2 #: " ++ int_to_string (bookcount asks2 0) ++ " --- " ++ str ++ ": " ++ "(" ++ int_to_string exchid ++ "," ++ int_to_string os ++ ")"

>tracehd str [] = error str
>tracehd str (x:xs) = x

||>tracebookprices :: [order_t] -> [char]
||>tracebookprices [] = error "empty ask book"                      
||>tracebookprices (x:xs) = int_to_string (getorderprice x) ++ tracebookprices xs, if((hd xs) ~= [])
||>                       = int_to_string x, otherwise


============================
ORDER BOOK HELPER FUNCTIONS
============================

Here is a function to decide where to place Buy orders given two askbooks and a target order size
It takes args (i) askbook (ii) askbook (iii) ordersize for ex1 (iv) ordersize for ex2 (v) targetordersize

>decideorders :: [order_t]  -> [order_t] -> num -> num -> num -> [(num, num)]
>decideorders as1      as2      s1 s2 0   = [(0,s1), (1,s2)]
>decideorders []       []       s1 s2 inv = [(0,s1), (1,s2)]
>decideorders []       (a2:as2) 0  s2 inv = [(0,0), (1,s2+min[totsize,inv])]
>                                           where
>                                           totsize = foldr (+) 0 (map getordersize (a2:as2))
>decideorders []       as2      s1 s2 inv = [(0,s1), (1,s2+inv)]
>decideorders (a1:as1) []       s1 0  inv = [(0,s1+min[totsize,inv]), (1,0)]
>                                           where
>                                           totsize = foldr (+) 0 (map getordersize (a1:as1))
>decideorders as1      []       s1 s2 inv = [(0,s1+inv), (1,s2)]
>decideorders (a1:as1) (a2:as2) s1 s2 inv = decideorders as1 (a2:as2) (s1+ss1) s2 (inv-ss1), if (p1<p2)||((p1=p2)&(ss1>=ss2))
>                                         = decideorders (a1:as1) as2 s1 (s2+ss2) (inv-ss2), otherwise
>                                           where
>                                           p1  = getorderprice a1
>                                           p2  = getorderprice a2
>                                           ss1 = getordersize a1
>                                           ss2 = getordersize a2



This function takes the lowest value from a list of numbers

>selectmin f [] = error "selectmin applied to empty list"
>selectmin f xs = foldl g (hd xs) xs
>                 where
>                 g a b = a, if(f a < f b)
>                       = b, otherwise


This function returns an ordersize for the first askbook before the ask price becomes worst than the second askbook

>volumeatexch::[order_t] -> [order_t] -> num -> num
>volumeatexch (x:xs) (y:ys) vol = vol + (getordersize x) + (volumeatexch xs (y:ys) vol), if ((getorderprice x) < (getorderprice y))
>                               = vol, otherwise

Get order size from 1 item in the list result from decide orders

>os :: (num,num) -> num
>os (exchid,ordersize) = ordersize

Get order exchange id from 1 item in the list result from decide orders 

>oex :: (num,num) -> num
>oex (exchid,ordersize) = exchid

>getbrokersize :: orderid_t -> num
>getbrokersize (Id i) = 7000, if(i=1)
>                     = 7000, if(i=4)
>                     = 1800, if(i=5)
>                     = error "getbrokersize: no matching broker id", otherwise

====================
AGENT TYPE
====================
All trading agents must have the same type

>agent_t * == ([*],[*],[*],[*]) -> ([exchoutput_t],[exchoutput_t]) -> agent_state_t -> startstoptime_t -> time_t -> id_t -> agentoutput_t
>agentoutput_t ::= AgentOP [[ordertuple_t]]
>ordertuple_t ::= NoOrderTuple id_t (inv_t,cash_t) | Ordertuple id_t exchid_t (order_t, order_t, order_t, order_t, (inv_t,cash_t))
>agent_state_t ::= AgentState inv_t cash_t internal_state_t
>inv_t == num
>cash_t == num
>internal_state_t == (num, num)
>startstoptime_t == num
>time_t == num
>id_t == orderid_t

>isvalidordertuple (NoOrderTuple id (a,b)) = False
>isvalidordertuple (Ordertuple id xid (a,b,c,d,invs)) = True

====================
BROKER AGENT DETAILS
====================

The broker has a fixed size order, from a client that needs to be executed as market orders only. 
i.e that it will try to fill the order in its entirety at current best price, in blocks, across both exchanges. 
When one best ask has been filled, it will move onto the next best ask until the entire order has been filled.
 
>broker :: agent_t *
>broker xq ([],ys) (AgentState oldinv oldcash (oldprice,oldsize)) starttime time id = AgentOP []
>broker xq (xs,[]) (AgentState oldinv oldcash (oldprice,oldsize)) starttime time id = AgentOP []
>broker xq (((ExchOP (bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1,bbooks1,abooks1)):rest1),
>           ((ExchOP (bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2,bbooks2,abooks2)):rest2)) 
>          (AgentState oldinv oldcash (oldprice,oldsize)) starttime time id
>              = f order_for_timestep (broker xq (rest1, rest2) (AgentState newinv newcash (0,0)) starttime (time+1) id)
>                where
>                f xs (AgentOP ys)  = AgentOP (xs:ys)
>                order_for_timestep = [Ordertuple id 0 (bid1, ask1, sell1, buy1, (newinv,newcash)), Ordertuple id 1 (bid2, ask2, sell2, buy2, (newinv,newcash))], 
>                                     if (oldinv=0) & (time>=starttime) || ((oldinv < broker_mo_size)&(time>=starttime))
>                                   = [NoOrderTuple id (newinv,newcash), NoOrderTuple id (newinv,newcash)], otherwise
>                bid1 = NoOrder
>                bid2 = NoOrder
>                ask1 = NoOrder || Order Ask 0 0 id time 0
>                ask2 = NoOrder || Order Ask 0 0 id time 1
>                buy1 = Order Buy buysizeA 0 id time 0 || 1234 || (buysize1 (decideorders asks1 asks2 0 0 broker_mo_size)) 0 id time 0
>                buy2 = Order Buy buysizeB 0 id time 1 || 5678 || (buysize2 (decideorders asks1 asks2 0 0 broker_mo_size)) 0 id time 1
>                [(exidA, buysizeA), (exidB, buysizeB)] = decideorders (tracehd "broker:abooks1" abooks1) (tracehd "broker:abooks2" abooks2) 0 0 (getbrokersize id)
>                sell1 = Order Sell 0 0 id time 0 
>                sell2 = Order Sell 0 0 id time 1
>                newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                psi (Id i) os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>                psi NoId   os = error "broker:psi: no order Id"
>                newcash = oldcash - (phi id xbids1) - (phi id xbuys1) + (phi id xasks1) + (phi id xsells1)
>                                  - (phi id xbids2) - (phi id xbuys2) + (phi id xasks2) + (phi id xsells2)
>                phi NoId   os = error "frontrunner:phi: no order id"
>                phi (Id i) os = foldr (+) 0 (f (map getorderprice (filter ((=i).getorderid) os)) (map getordersize (filter ((=i).getorderid) os)))
>                                where
>                                f [] [] = []
>                                f xs [] = []
>                                f [] ys = []
>                                f (x:xs) (y:ys) = (x*y):(f xs ys)

=======================
POPULATOR AGENT DETAILS
=======================

The populator agent issues ask orders at various prices for 10 timesteps, 
up until the broker agent begins issuing buy orders. This creates liquidity
for this experinent


>populator :: agent_t *
>populator xq ((ExchOP (bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1,bbooks1,abooks1):rest1),
>              (ExchOP (bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2,bbooks2,abooks2):rest2)) 
>             (AgentState oldinv oldcash (oldprice,oldsize)) stoptime time id || Make sure stoptime = startime-1
>                = f [Ordertuple id exchid (bid, ask, sell, buy, (newinv,newcash)), NoOrderTuple id (newinv,newcash)] 
>                    (populator xq (rest1, rest2) (AgentState newinv newcash (0,0)) stoptime (time+1) id), if exchid=0
>                = f [NoOrderTuple id (newinv,newcash), Ordertuple id exchid (bid, ask, sell, buy, (newinv,newcash))] 
>                    (populator xq (rest1, rest2) (AgentState newinv newcash (0,0)) stoptime (time+1) id), if exchid=1
>                = error "populator - bad exchange id", otherwise
>                  where
>                  f xs (AgentOP ys) = AgentOP (xs:ys)
>                  bid = Order Bid 0 0 id time exchid
>                  ask = Order Ask (populator_sizes!time) (populator_prices!time) id time exchid, if (time <= stoptime)
>                      = Order Ask 0 0 id time exchid, otherwise
>                  buy = Order Buy 0 0 id time exchid
>                  sell = Order Sell 0 0 id time exchid 
>                  exchid = 0, if ((time mod 2) = 0)
>                         = 1, otherwise
>                  newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                  psi (Id i) os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>                  psi NoId   os = error "populator:psi: no ID"
>                  newcash = oldcash - (phi id xbids1) - (phi id xbuys1) + (phi id xasks1) + (phi id xsells1)
>                                  - (phi id xbids2) - (phi id xbuys2) + (phi id xasks2) + (phi id xsells2)
>                  phi NoId   os = error "frontrunner:phi: no order id"
>                  phi (Id i) os = foldr (+) 0 (f (map getorderprice (filter ((=i).getorderid) os)) (map getordersize (filter ((=i).getorderid) os)))
>                                  where
>                                  f [] [] = []
>                                  f xs [] = []
>                                  f [] ys = []
>                                  f (x:xs) (y:ys) = (x*y):(f xs ys)

==========================
FRONT RUNNER AGENT DETAILS
==========================


The frontrunner agent monitors both exchanges, and issues very small (often 1 share) ask orders at the best price, in order to
determine their counterparty. Once that has been done, they look at the historical average order size for the counterparty
and look to pre empt their order at best ask price across exchanges, thus giving them the ability to then sell the order back
to the counterparty at a higher price. This is known as front-running.

We will need to track the profit for the frontrunner; the frontrunner will start with (say) 1,000,000 cash and 0 inventory; at each time step it must check its own executed buys and asks
and use the received information to update its cash and inventory postions.  The cash position should be communicated to the exchange (perhaps pretending to be "inv"?) so that the cash
position can be printed out at each timestep.  The new cash and inventory positions must be passed on to the recursive call to frontrunner.  Frontrunner will of course need an extra argument, but 
this means that ALL trading agents must have an extra argument (and the definition of agent_t must be extended).

>frontrunner :: agent_t *
>frontrunner xq ((ExchOP (bids1,asks1,sells1,buys1,xbids1,xasks1,xsells1,xbuys1,bestbid1,bestask1,ltp1,pp1,sellp1,buyp1,invs1,bbooks1,abooks1):rest1),
>                (ExchOP (bids2,asks2,sells2,buys2,xbids2,xasks2,xsells2,xbuys2,bestbid2,bestask2,ltp2,pp2,sellp2,buyp2,invs2,bbooks2,abooks2):rest2)) 
>               (AgentState oldinv oldcash (oldprice, oldsize)) starttime time id
>                  = f order_for_timestep (frontrunner xq (rest1, rest2) (AgentState newinv newcash (newprice,newsize)) starttime (time+1) id)
>                    where
>                    f xs (AgentOP ys) = AgentOP (xs:ys)
>                    estsize = broker_mo_size
>                    estsize2 = broker_mo_size_2
>                    ||
>                    || THIS HAD BAD LOGIC !!!
>                    ||
>                    || order1size may be filled by several executions!
>                    ||
>                    || also - frontrunner should be looking at the askbooks from a previous timestep (not the current timestep)
>                    ||      - frontrunner needs to look at the askbook at time t - (2 + exch->brokerdelay + broker->exchdelay)
>                    ||      - to start with, assume exch->brokerdelay = broker->exchdelay = 0
>                    ||        so we need to provide decideorders with abooks1!2 and abooks2!2
>                    ||
>                    [(exch1id,exch1size),(exch2id,exch2size)] = decideorders (index 2 abooks1) (index 2 abooks2) 0 0 estsize
>                    [(exch1id_2,exch1size_2),(exch2id_2,exch2size_2)] = decideorders (index 2 abooks1) (index 2 abooks2) 0 0 estsize2
>                    index n [] = error "frontrunner:index"
>                    index 0 (x:xs) = x
>                    index n (x:xs) = index (n-1) xs   
>                    order1size = exch1size
>                    order1size_2 = exch1size_2
>                    order_for_timestep = error "frontrunner: one", if (xbuys1 ~=[]) &
>                                                                      (order1size=(foldr (+) 0 (map getordersize xbuys1))) & || (getordersize (tracehd "frontrunner:buy_exch1" xbuys1))) &
>                                                                      (time>=starttime) &
>                                                                      (dotrade=False)
>                                       = error "frontrunner: two", if (xbuys1 ~=[]) &
>                                                                      (order1size=(foldr (+) 0 (map getordersize xbuys1))) & || (getordersize (tracehd "frontrunner:buy_exch1" xbuys1))) &
>                                                                      (time<starttime) &
>                                                                      (dotrade=True)
>                                       = error ("frontrunner: three. order1size="++(shownum order1size)++" xbuys1="++(shownum (foldr (+) 0 (map getordersize xbuys1)))), 
>                                                                   if (xbuys1 ~=[]) &
>                                                                      (order1size~=(foldr (+) 0 (map getordersize xbuys1))) & || (getordersize (tracehd "frontrunner:buy_exch1" xbuys1))) &
>                                                                      (time>=starttime) &
>                                                                      (dotrade=True)
>                                      = [NoOrderTuple id (newinv,newcash), 
>                                           Ordertuple id 1 (bid, ask, buy, sell, (newinv,newcash))], 
>                                                                  if (xbuys1 ~= []) &
>                                                                      ((order1size=(foldr (+) 0 (map getordersize xbuys1)))\/
>                                                                       (order1size_2=(foldr (+) 0 (map getordersize xbuys1)))) & || (getordersize (tracehd "frontrunner:buy_exch1" xbuys1))) &
>                                                                      (time>=starttime) &
>                                                                      (dotrade=True) &
>                                                                      (oldprice=0)
>                                     = [NoOrderTuple id (newinv,newcash), 
>                                          Ordertuple id 1 (bid, ask, buy, sell, (newinv,newcash))], 
>                                                                   if (oldprice~=0)
>                                     = [NoOrderTuple id (newinv,newcash), NoOrderTuple id (newinv,newcash)], otherwise
>                    newprice = 0, if (oldprice~=0)
>                             = atprice, if (oldprice=0) & (isvalidordertuple (order_for_timestep!1))   || we have to remember previous best price to use for ask at next time step
>                             = oldprice, otherwise
>                    newsize  = 0, if (oldsize~=0)
>                             = exch2sizet, if(oldsize=0) & (isvalidordertuple (order_for_timestep!1))
>                             = oldsize, otherwise
>                    bid  = NoOrder 
>                    buy  = Order Buy exch2sizet 0       id time 1, if (oldprice=0)
>                         = NoOrder, otherwise
>                    ask  = Order Ask oldsize oldprice id time 1, if (oldprice~=0)
>                         = NoOrder, otherwise
>                    sell = NoOrder
>                    (dotrade,atprice) = newbestask (index 2 abooks2) exch2sizet
>                                        where
>                                        index n [] = error "frontrunner:newbestask:index"
>                                        index 0 (x:xs) = x
>                                        index n (x:xs) = index (n-1) xs
>                    || newbestask calculates both whether it is profitable to front run and if so what the new ask price should be
>                    || if the existing best ask on the book is bigger than the buy size, the frontrunner can't make a profit!
>                    ||
>                    ||newbestask :: [order_t] -> num -> (bool, num)
>                    exch2sizet = exch2size, if(order1size=(foldr (+) 0 (map getordersize xbuys1)))
>                               = exch2size_2, if(order1size_2=(foldr (+) 0 (map getordersize xbuys1)))
>                               = error "frontrunner:exch2size: no order matched", otherwise
>                    newbestask []      buysize = (True,  ltp2),                 if (ltp2~=0)
>                                               = (True,  startprice), otherwise
>                    newbestask (x:xs)  buysize = (False, 0),                    if (getordersize x) > buysize
>                                               = (True, nba (x:xs) buysize), otherwise
>                                                 where
>                                                 nba []     buysize = error "newbestask"
>                                                 nba (x:xs) buysize = (getorderprice x) - 1,                         if (getordersize x) > buysize
>                                                                    = getorderprice (hd xs) - 1,                     if ((getordersize x) = buysize) & (xs ~= [])
>                                                                    = getorderprice x,                               if (getordersize x) = buysize
>                                                                    = getorderprice x,                               if (xs=[])
>                                                                    = nba xs (buysize - (getordersize x)),           otherwise
>                    newinv = oldinv + (psi id xbids1) + (psi id xbuys1) - (psi id xasks1) - (psi id xsells1) 
>                                    + (psi id xbids2) + (psi id xbuys2) - (psi id xasks2) - (psi id xsells2)
>                    psi NoId   os = error "frontrunner:psi: no order id"
>                    psi (Id i) os = foldr (+) 0 (map getordersize (filter ((=i).getorderid) os))
>                    newcash = oldcash - (phi id xbids1) - (phi id xbuys1) + (phi id xasks1) + (phi id xsells1)
>                                      - (phi id xbids2) - (phi id xbuys2) + (phi id xasks2) + (phi id xsells2)
>                    phi NoId   os = error "frontrunner:phi: no order id"
>                    phi (Id i) os = foldr (+) 0 (f (map getorderprice (filter ((=i).getorderid) os)) (map getordersize (filter ((=i).getorderid) os)))
>                                    where
>                                    f [] [] = []
>                                    f xs [] = []
>                                    f [] ys = []
>                                    f (x:xs) (y:ys) = (x*y):(f xs ys)


==========================
EXCHANGE AGENT DETAILS
==========================

The exchange takes in lists of orders from the traders and produces the bestbid and bestask plus 
lists of confirmations (and price, inventories, sellp and buyp for the graphs).   
The exchange also takes in and outputs its own bidbook and askbook.  The exchange has id 0. 
 
>exchoutput_t ::= ExchOP ([order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],[order_t],num,num,num,[num],num,num,[(num,num)],[[order_t]],[[order_t]])
>exch:: num -> [([order_t],[order_t],[order_t],[order_t],[(num,num)])]->num->[[order_t]]->[[order_t]] -> num 
>           -> [exchoutput_t]
>exch id ((allbids,allasks,allsells,allbuys,invs):rest) time bbooks abooks ltp 
>    = (ExchOP (bids,asks,sells,buys,xbids, xasks, xsells, xbuys, bb, ba, newltp, pp, sellp, buyp, invs, newbbooks, newabooks)):  (exch id rest (time+1) newbbooks newabooks newltp) 
>      where 
>      bids = (filter ((=id).getorderexchid) (filter (~=NoOrder) allbids))
>      asks = (filter ((=id).getorderexchid) (filter (~=NoOrder) allasks))
>      sells = (filter ((=id).getorderexchid)(filter (~=NoOrder) allsells))
>      buys = (filter ((=id).getorderexchid) (filter (~=NoOrder) allbuys))
>      bbook1 = insb bids [], if allfillandkill 
>             = insb bids [], if bbooks=[]
>             = insb bids (tracehd "exch:bbooks" bbooks), otherwise 
>               where 
>               insb []     []     = [] 
>               insb []     ys     = ys 
>               insb (x:xs) []     = insb xs [], if (getordersize x) = 0 
>                                  = insb xs [x], otherwise 
>               insb (x:xs) (y:ys) = insb xs (y:ys), if (getordersize x) = 0 
>                                  = insb xs (x:(y:ys)), if (getorderprice x) > (getorderprice y) 
>                                  = y:(insb (x:xs) ys), otherwise 
>      abook1 = insa asks [], if allfillandkill 
>             = insa asks [], if abooks=[]
>             = insa asks (tracehd "exch:abooks" abooks), otherwise 
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
>      newbbooks = (newbbook:bbooks)
>      newabooks = (newabook:abooks)
>      (xbids, xasks) = (xbids1++xbids2, xasks1++xasks2) 
>      newltp = min (map getorderprice xbids), if xbids ~= [] 
>             = max (map getorderprice xasks), if xasks ~= [] 
>             = ltp, otherwise 
>      sellp = (foldr (+) 0 (map getordersize sells)) /(1+ (foldr (+) 0 (map getordersize bbook1))) 
>      buyp = (foldr (+) 0 (map getordersize buys)) / (1+ (foldr (+) 0 (map getordersize abook1))) 
>      bb = 0, if (newbbook = []) 
>         = getorderprice (tracehd "exch:bb" newbbook), otherwise 
>      ba = 0, if (newabook = []) 
>         = getorderprice (tracehd "exch:ba" newabook), otherwise 
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
The function "sim" takes a number of timesteps and returns a pair of exchange outputs (one for each exchange)
 
>sim::num->([exchoutput_t],[exchoutput_t])
>sim steps = f allexchstates steps 
>            where 
>            f (a, b) steps = ((take steps a), (take steps b))
>            ||
>            || allexchstates is a two-tuple created by calling the two exchanges
>            || each exchange output is prepended with an initial state
>            || each exchange function is applied to an ID (0 or 1), the combined messages from all agents (allmessages),
>            || a start time (1), two initial states (both []) and a startprice for the market 
>            ||
>            || allexchstates :: (exchoutput_t, exchoutput_t)
>            allexchstates = (initexchstate:(exch 0 mergedmessages 1 [] [] startprice), initexchstate:(exch 1 mergedmessages 1 [] [] startprice)) 
>            ||
>            || initexchstate is the initial state prepended to both exchange outputs
>            || initexchstate :: exchoutput_t
>            ||
>            initexchstate = ExchOP (  [],  [],  [],   [],  [],   [],   [],    [],   startprice-(startspread/2),startprice+(startspread/2),startprice,[], 0,    0,   [],  [[]],   [[]])  
>            ||                        bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,                        ba,                        ltp,        pp,sellp,buyp,invs,bbooks,abooks
>            ||
>            || mergedmessages :: [([order_t],[order_t],[order_t],[order_t],[num])]
>            || mergedmessages is the result of transposing the list of agent outputs and then grouping the messages
>            || the messages are grouped into a 5-tuple of bids, asks, sells, buys, and invs
>            || thus, the resulting list has as its first item a 5-tuple containing all bids, asks, sells, buys and invs sent in the first time step
>            ||
>            mergedmessages = map (mygroup initgroups) (mytranspose tradermessages)
>                             where 
>                             ||
>                             || tradermessage is a list of outputs from the agents
>                             || constructed by zipping partial applications of the agents with their IDs
>                             || this creates a list of two-tuples, each containing a partial application and an ID
>                             || then a local function "g" is mapped over the list ("g" supplies the final two args to each agent function)
>                             || this gives a list containing the outputs from every output
>                             || tradermessage :: [agentoutput_t]
>                             ||
>                             tradermessages = map g (zip2 (agents allexchstates) (map Id [1..])) 
>                                              where 
>                                              g ((ex1d,ex2d,f),id) = AgentOP (map h (myzip ex1stream ex2stream))
>                                                                     where
>                                                                     agentmessages = f 0 id
>                                                                     ex1stream = (rep ex1d (NoOrderTuple id (0,0)))++(map hd      (strip agentmessages))
>                                                                     ex2stream = (rep ex2d (NoOrderTuple id (0,0)))++(map (hd.tl) (strip agentmessages))
>                                                                     h (a,b) = [a,b]
>                                                                     strip (AgentOP xs) = xs
>                                                                     myzip [] [] = []
>                                                                     myzip xs [] = myzip xs (rep (#xs) (NoOrderTuple id (0,0)))
>                                                                     myzip [] ys = myzip (rep (#ys) (NoOrderTuple id (0,0))) ys
>                                                                     myzip (x:xs) (y:ys) = (x,y):(myzip xs ys)
>                                                                     
>                             number_of_traders = # tradermessages
>                             ||
>                             || the function mytranspose is a customised version of the standard function transpose (see Sec 28 of the manual)
>                             || NB the agent outputs are of type agentoutput_t and so we use functions "agentophd" and "agentoptl" to get the heads and tails.
>                             || it terminates early if any agent stops sending output
>                             || the result is a (possibly infinite) list where the first item is all the messages to the exchanges at the first timestep,
>                             || and the next item is those messages at the second timestep, and so on
>                             ||
>                             mytranspose []  = []
>                             mytranspose any = [], if (member any (AgentOP [])) || if any trader has stopped issuing messages, we halt the simulation
>                                             = (map agentophd any) : (mytranspose (map agentoptl any)), otherwise
>                                               where
>                                               agentophd (AgentOP []) = error "found empty list in agentophd, subdef of mytranspose"
>                                               agentophd (AgentOP xs) = hd xs
>                                               agentoptl (AgentOP []) = error "found empty list in agentophd, subdef of mytranspose"
>                                               agentoptl (AgentOP xs) = AgentOP (tl xs)
>                             ||
>                             || the function mygroup is applied to the transposed tradermessages to create the mergedmessages
>                             || it is applied to a list of outputs from all agents at one timestep - each agent issues a list of ordertuples
>                             || so mygroup is applied to a list of lists of ordertuples
>                             || it takes the agent output WITHOUT the AgentOP constructor - this should be a list of  ordertuple_t
>                             || Notice that after grouping the Ordertuple constructor is lost - so the exchange sees raw bids, asks etc
>                             ||           the groups are initialised to be empty
>                             ||           (bids, asks, sells, buys, invs)  
>                             ||
>                             initgroups = ([],   [],   [],    [],   [])
>                             ||
>                             mygroup acc xs = process (xmygroup acc xs)
>                                              where
>                                              process (bids,asks,sells,buys,xs) = (bids,asks,sells,buys,(xprocess xs number_of_traders 1))
>                                                                                  where
>                                                                                  xprocess xs 0 i = []
>                                                                                  xprocess xs n i = (q (filter (p i) xs)): (xprocess xs (n-1) (i+1))
>                                                                                  p i ((Id id),a,b) = (i=id)
>                                                                                  p i (NoId   ,a,b) = error "sim:mygroup:process:p: NoId"
>                                                                                  q [] = error "sim:mygroup:process:q: empty list"
>                                                                                  q ((id,a,b):rest) = (a,b)
>                                              xmygroup acc []     = acc
>                                              xmygroup acc (x:xs) = xmygroup (g acc x) xs
>                                              g (bids, asks, sells, buys, invs) []                            = (bids, asks, sells, buys, invs)
>                                              g (bids, asks, sells, buys, invs) ((NoOrderTuple id (a,b)):rest)        
>                                                                                                              = g ((bids++[NoOrder]), 
>                                                                                                                   (asks++[NoOrder]), 
>                                                                                                                   (sells++[NoOrder]), 
>                                                                                                                   (buys++[NoOrder]), 
>                                                                                                                   (invs++[(id,a,b)])) rest
>                                              g (bids, asks, sells, buys, invs) ((Ordertuple id xid (bi,a,s,bu,(i,c))):rest) 
>                                                                                                              = g ((bids++[bid]), (asks++[ask]), 
>                                                                                                                  (sells++[sell]), (buys++[buy]), (invs++[(id,i,c)])) rest
>                                                                                                                where
>                                                                                                                bid  = safehd (filter ((=Bid).getordertype) [bi,a,s,bu])
>                                                                                                                ask  = safehd (filter ((=Ask).getordertype) [bi,a,s,bu])
>                                                                                                                sell = safehd (filter ((=Sell).getordertype) [bi,a,s,bu])
>                                                                                                                buy  = safehd (filter ((=Buy).getordertype) [bi,a,s,bu])
>                                                                                                                safehd []     = NoOrder
>                                                                                                                safehd (x:xs) = x
 

And now finally we need to output the sim results to file 
 
>runtest steps = [Tofile "simplesim12mm.csv" (hdrs ++ (g startprice 0 (sim steps))), 
>                 Closefile "simplesim2mm.csv", 
>                 Stdout "/Applications/Microsoft\\ Office\\ 2011/Microsoft\\ Excel.app/Contents/MacOS/Microsoft\\ Excel &" ] 
>                where 
>                ||hdrs = "Price,SellPressure,BuyPressure,Inv5,Inv4,Inv3,Inv2,Inv1" 
>                hdrs = "Time,Bids,Asks,Sells,Buys,BidPrice1,BidPrice2,AskPrice1,AskPrice2,AskSize1,Asksize2,BuySize1,BuySize2,SellSize1,SellSize2,"
>                       ++ "Price,XBuys,SellPressure,BuyPressure,Net Pressure,BestBid,BestAsk,TradedPrice,TradedSize,BuyerID,AskerID,InvBrok,CashBrok,InvPop,CashPop,InvFR,CashFR,Abook\n"
>                g p t ([], [])         = []
>                g p t ([], b)          = []
>                g p t (a, [])          = []
>                g p t ((a:aa), (b:bb)) = output ++ (g ltp (t+1) (aa, bb)) 
>                                         where 
>                                         output = output1 ++ output2
>                                         ltp = ltp2          || chosen arbritrarily
>                                         (output1,ltp1) = h p t a
>                                         (output2,ltp2) = h p t b
>                h p t (ExchOP (bids,asks,sells,buys,xbids,xasks,xsells,xbuys,bb,ba,ltp,pp,sellp,buyp,invs,bbooks,abooks))
>                         = k
>                             (foldr (+) 0 (map getordersize bids))  || Bids
>                             (foldr (+) 0 (map getordersize asks))  || Asks
>                             (foldr (+) 0 (map getordersize sells))  || Sells
>                             (foldr (+) 0 (map getordersize buys))  || Buys
>                             ((getorderprice.safehd) (filter ((=1).getorderid) bids))  || BidPrice1
>                             ((getorderprice.safehd) (filter ((=2).getorderid) bids))  || BidPrice2
>                             ((getorderprice.safehd) (filter ((=1).getorderid) asks))  || AskPrice1
>                             ((getorderprice.safehd) (filter ((=2).getorderid) asks))  || AskPrice2
>                             ((getordersize.safehd)  (filter ((=1).getorderid) asks))   || AskSize1
>                             ((getordersize.safehd)  (filter ((=2).getorderid) asks))   || AskSize2
>                             ((getordersize.safehd)  (filter ((=1).getorderid) buys))   || BuySize1
>                             ((getordersize.safehd)  (filter ((=2).getorderid) buys))   || BuySize2
>                             ((getordersize.safehd)  (filter ((=1).getorderid) sells))  || SellSize1
>                             ((getordersize.safehd)  (filter ((=2).getorderid) sells))  || SellSize2
>                             (foldr (+) 0 (map getordersize xbuys))  || XBuys
>                             sellp || SellPressure
>                             buyp  || BuyPressure
>                             bb    || BestBid
>                             ba    || BestAsk
>                             invs  || Inventories and Cash
>                             pp    || Prices
>                             ltp   || LastTradePrice
>                             (getorderprice (safehd xbuys))
>                             (getordersize  (safehd xbuys))
>                             (getorderid  (safehd xbuys))
>                             (getorderid (safehd xasks))
>                             (checkbooks abooks) || Ask Book
>                           where 
>                           checkbooks [] = []
>                           checkbooks (x:xs) = x
>                           safehd [] = Order Bid 0 0 (Id 0) 0 0
>                           safehd (x:xs) = x 
>                           showwithcommas []     = "" 
>                           showwithcommas [x]    = (shownum x) 
>                           showwithcommas (x:xs) = (shownum x)++","++(showwithcommas xs) 
                          
                            || k  =
                            ||     bi = bids
                            ||     as = asks
                            ||     bu = buys
                            ||     se = sells
                            ||     bi1 = bid price 1
                            ||     bi2 = bid price 2
                            ||     as1 = ask price 1
                            ||     as2 = ask price 2
                            ||     asize1 = ask size 1
                            ||     asize2 = ask size 2
                            ||     bu1 = buys size 1
                            ||     bu2 = buy size 2
                            ||     se1 = sell size 1
                            ||     se2 = sell size 2
                            ||     xb  = executed buys
                            ||     sp = sell pressure
                            ||     bp = buy pressure
                            ||     bb = best bid
                            ||     ba = best ask
                            ||     is = inventories
                            ||     xs = prices
                            ||     ltp = last trade price
                            ||     tp = tradeprice
                            ||     ts = tradesize
                            ||     buyid = buyerid
                            ||     askid = askerid
                            ||     abk = ask book

>                           explode [] = []
>                           explode ((a,b):rest) = a:(b:(explode rest))
>                           k  bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is [] ltp tp ts buyid askid abk
>                              = ((showwithcommas (  [t,bi,as,se,bu,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,ltp,xb,sp,bp,(sp-bp),bb,ba,tp,ts,buyid,askid]
>                                                  ++(explode is)
>                                                  ++(map getordersize abk)))++"\n",ltp) 
>                           k  bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is xs ltp tp ts buyid askid abk
>                              = (kk bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is xs tp ts buyid askid abk, ltp) 
>
>
>                           kk       bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is [] tp ts buyid askid abk = error "kk applied to []" 
>
>                           kk       bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is [x] tp ts buyid askid abk 
>                              = (showwithcommas (  [t,bi,as,se,bu,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,x,xb,sp,bp,(sp-bp),bb,ba,tp,ts,buyid,askid]
>                                                 ++(explode is)
>                                                 ++(map getordersize abk)))++"\n" 
>                           kk       bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is (x:xs) tp ts buyid askid abk 
>                              = ((showwithcommas (  [t,bi,as,se,bu,bi1,bi2,as1,as2,asize1,asize2,bu1,bu2,se1,se2,x,xb,sp,bp,(sp-bp),bb,ba,tp,ts,buyid,askid]
>                                                  ++(explode is)
>                                                  ++(map getordersize abk)))++"\n") 
>                                ++
>                                || (kk bi as se bu bi1 bi2 as1 as2 asize1 asize2 bu1 bu2 se1 se2 xb sp bp bb ba is xs abk)  
>                                (kk 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 [] xs 0 0 0 0 [])  
 
 
For each experiment we need to specify some agents as follows. This is a list of partial applications of the function "mm";  
the function "sim" will apply each of these to its start time and to its unique agent id  
(NB I changed the order of the arguments to mm and fs so this would work): 
 
>agents agentinput = [
>                     (brokerexch1delay, brokerexch2delay,(broker      initxq agentinput (AgentState 0 0 (0,0))  buyer_start_time)),
>                     (popexch1delay, popexch2delay,      (populator   initxq agentinput (AgentState 0 0 (0,0)) (buyer_start_time-1))),
>                     (frontexch1delay, frontexch2delay,  (frontrunner initxq agentinput (AgentState 0 0 (0,0))  buyer_start_time)),
>                     (broker2exch1delay, broker2exch2delay,(broker    initxq agentinput (AgentState 0 0 (0,0))  (buyer_start_time+2)))
>                    ] 
>                    where 
>                    initxq = ([],[],[],[])
>                    (brokerexch1delay, brokerexch2delay) = (0,7)
>                    (popexch1delay, popexch2delay)       = (0,0)
>                    (frontexch1delay, frontexch2delay)   = (0,0)
>                    (broker2exch1delay, broker2exch2delay) = (0,2)
 
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
>populator_sizes = [400,400,1750,1200,1500,1600,920,950,1400,1175]
>populator_prices = [48,57,62,68,71,75,52,65,54,66]
>broker_mo_size = 7000
>broker_mo_size_2 = 9000
>debug_asks = True
>tester []     []     a1 a2 t = (a1,a2)
>tester []     b      a1 a2 t = error "argh"
>tester a      []     a1 a2 t = error "argh"
>tester (p:ps) (y:ys) a1 a2 t = ((Order Ask y p NoId 0 0):aa1, aa2), if (t mod 2) = 0
>                             = (aa1,(Order Ask y p NoId 0 0):aa2), otherwise
>                               where
>                               (aa1,aa2) = tester ps ys a1 a2 (t+1)
>||testme = decideorders (mysort ab1) (mysort ab2) 0 0 7000
>||         where
>||         (ab1, ab2) = tester populator_prices populator_sizes [] [] 0
>||         ps = [500,400,600,500]
>||         ss = [1000,1000,1000,1000]
>||         mysort xs = isort f xs []
>||                     where
>||                     f a b = (getorderprice a) <= (getorderprice b)
>||                     isort f []     acc    = acc
>||                     isort f (x:xs) []     = isort f xs [x]
>||                     isort f (x:xs) (y:ys) = isort f xs (x:(y:ys)), if (f x y)
>||                                           = y:(isort f (x:xs) ys), otherwise
>      
