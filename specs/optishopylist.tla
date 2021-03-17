--------------------------- MODULE optishopylist ---------------------------

EXTENDS TLC, Integers, FiniteSets, Sequences, Reals

CONSTANTS PRODUCTS, APPS, IDs, GATEAPPS
ASSUME Cardinality(APPS) > 0

PT == INSTANCE PT

set ++ item == set \union {item} 
set -- item == set \ {item}

(***************************************************************************)
(* In shoppying list, the product is in fact the identifier.               *)
(* Any item could have an information for how much of a product one        *)
(* wants to buy (not relevant in this specification).                      *)
(***************************************************************************)
ShopyItems == [id: PRODUCTS, bought: BOOLEAN]

ADD_ACTION == "add"
RM_ACTION == "rm"
SET_BOUGHT_ACTION == "set_bought"
REQ_SYNC_ACTION == "req_sync"
RESP_SYNC_ACTION == "resp_sync"
END_SYNC_ACTION == "end_sync"

(***************************************************************************)
(* Actions is the set of all possible actions in the system.               *)
(***************************************************************************)
Actions == {
    ADD_ACTION, 
    RM_ACTION, 
    SET_BOUGHT_ACTION, 
    REQ_SYNC_ACTION, 
    RESP_SYNC_ACTION,
    END_SYNC_ACTION
}

SyncActions == {REQ_SYNC_ACTION, RESP_SYNC_ACTION}

(***************************************************************************)
(* SyncMsgs is the set of all possible messages sent for synchronisation   *)
(* of shopy lists.                                                         *)
(***************************************************************************)
SyncMsgs ==
    [id: IDs,
     app: APPS, 
     list: SUBSET ShopyItems, 
     mergedList: SUBSET ShopyItems,
     type: SyncActions]

(***************************************************************************)
(* Messages sent for joining the network.                                  *)
(***************************************************************************)
JoinRespMsgs ==
    [app: APPS,
     knownHosts: PT!SeqOf(APPS, Cardinality(APPS))]

JoinReqMsgs ==
    [app: APPS]

(***************************************************************************)
(* Messages sent for notifications about new joiners in the network.       *)
(***************************************************************************)
JoinNotifMsgs ==
    [app: APPS]

(***************************************************************************)
(* The spec now depicts a shopping-list app where the server app manages   *)
(* several users and hence multiple lists of items that synch eventually.  *)
(*                                                                         *)
(* The list contains unique items, thus we use a set.                      *)
(***************************************************************************)
(* --algorithm OptiShopyList

variable
    \* whether an app is a gate
    isGate = [a \in APPS |-> a \in GATEAPPS],
    \* one shopping list for all APPS
    shopyList = [a \in APPS |-> {}],
    \* sync shopyList requests/responses
    syncReqQueue = [a \in APPS |-> <<>>],
    syncRespQueue = [a \in APPS |-> <<>>],
    \* join to network requests/responses
    joinReqQueue = [a \in APPS |-> <<>>],
    joinRespQueue = [a \in APPS |-> <<>>],
    \* new joiner notifications
    newJoinerNotif = [a \in APPS |-> <<>>],
    \* set of taken IDs
    takenIDs = {};

define
    (***********************************************************************)
    (* A couple of helpers for shopy-list items                            *)
    (***********************************************************************)
    
    NewShopyItem(list) == 
        [id     |-> (CHOOSE x \in PRODUCTS: ~\E i \in list: x = i.id), 
         bought |-> FALSE]
         
    ExistingShopyItem(list) == CHOOSE x \in list: TRUE
    
    ExistingNotBoughtShopyItem(list) == CHOOSE x \in list: x.bought = FALSE
    
    (***********************************************************************)
    (* Helpers for Sync messages request/response.                         *)
    (***********************************************************************)
    
    NewSyncMsg(id, a, l, ml, t) ==
        [id |-> id,
         app |-> a, 
         list |-> l, 
         mergedList |-> ml,
         type |-> t]
         
    NewSyncReqMsg(a, l, ml, t) ==
        NewSyncMsg(
            (CHOOSE i \in IDs: \A ti \in takenIDs: i /= ti),
            a, l, ml, t
        )
        
    NewSyncReq(app) == 
        NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)
        
    NewSyncResp(app, mergeResult, id) == 
        NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)
    
    (***********************************************************************)
    (* Helpers for the decentralized network features.                     *)
    (***********************************************************************)
    
    NewJoinReqMsg(app) == [app |-> app]
    
    NewJoinRespMsg(app, hosts) ==
        [app |-> app,
         knownHosts |-> hosts]
    
    GateApps == {a \in APPS: isGate[a]}
    
    NewJoinerNotifReq(app) == [app |-> app]
    
    (***********************************************************************)
    (* Amongst 'knownApps', wisely choose an element different than 'app'. *)
    (***********************************************************************)
    PickGossipFriends(app, knownApps) ==
        LET KnownApps ==
                PT!ReduceSeq(
                    LAMBDA a, acc: IF a = app THEN acc ELSE Append(acc, a),
                    knownApps, <<>>)
        
            Opposit ==
                PT!Index(KnownApps, app) + (Len(KnownApps) \div 2) - (Len(KnownApps) % 2)
                
            PreviousIndex(i) ==
                IF Len(KnownApps) < 3
                THEN 1
                ELSE IF i = 1 THEN Len(KnownApps) ELSE i - 1
           
            NextIndex(i) ==
                IF Len(KnownApps) < 3
                THEN Len(KnownApps)
                ELSE IF i = Len(KnownApps) THEN 1 ELSE i + 1
                
        IN {KnownApps[PreviousIndex(Opposit)], 
            KnownApps[NextIndex(Opposit)]}
    
    (***********************************************************************)
    (* Merge two lists of apps together without duplicating                *)
    (* equal elements.                                                     *)
    (***********************************************************************)
    MergeKnownApps(apps1, apps2) ==
        LET AppSeq(n) == PT!SeqOf(APPS, n)
        
            Contains(appSeq, appItem) ==
                Cardinality(PT!Matching(appSeq, appItem)) > 0
                
            f[args \in AppSeq(Len(apps1))
                       \X AppSeq(Len(apps2))
                       \X AppSeq(Len(apps1) + Len(apps2))] ==
                       
                LET l1 == args[1]
                
                    l2 == args[2]
                    
                    acc == args[3]
                    
                    PickFromL1 == f[<<
                        Tail(l1),
                        l2,
                        Append(acc, Head(l1))>>]
                        
                    SkipOneL1 == f[<<
                        Tail(l1),
                        l2,
                        acc>>]
                        
                    PickFromL2 == f[<<
                        l1,
                        Tail(l2),
                        Append(acc, Head(l2))>>]
                        
                    SkipOneL2 == f[<<
                        l1,
                        Tail(l2),
                        acc>>]
                IN
                    IF Len(l2) = 0
                    THEN IF Len(l1) = 0 
                         THEN acc 
                         ELSE IF Contains(acc, Head(l1)) THEN SkipOneL1 ELSE PickFromL1
                    ELSE IF Head(l1) /= Head(l2)
                         THEN IF Contains(acc, Head(l2)) THEN SkipOneL2 ELSE PickFromL2
                         ELSE IF Contains(acc, Head(l1)) THEN SkipOneL1 ELSE PickFromL1
        IN f[<<apps1, apps2, <<>>>>]
end define;

macro notify(apps, newJoiner)
begin
    newJoinerNotif := [a \in APPS |-> 
        IF a \in apps
        THEN Append(newJoinerNotif[a], NewJoinerNotifReq(newJoiner))
        ELSE newJoinerNotif[a]];
end macro;

(***************************************************************************)
(* This proccess represents a shopy-list running                           *)
(* in one of the several network clients.                                  *)
(*                                                                         *)
(* Since Opti-shopylist is a decentralized program, the user creates a     *)
(* network of connected instances of Opti-shopylist.                       *)
(*                                                                         *)
(* We assume that every client app has only one shopy-list.                *)
(***************************************************************************)
fair+ process ClientApp \in APPS
variables
    joined = FALSE,
    gossipFriends = {},
    knownApps = <<self>>;

begin AppLoop:
    while TRUE do

        (*******************************************************************)
        (* MANAGE DECENTRALIZED NETWORK                                    *)
        (*******************************************************************)
        either
            \* SEND JOIN REQUEST
            (***************************************************************)
            (* A client app might send a join request to any available     *)
            (* gate app.                                                   *)
            (***************************************************************)
            await ~joined;
            with a \in (GateApps -- self)
            do
                joinReqQueue[a] := Append(joinReqQueue[a], NewJoinReqMsg(self));
            end with;
        or
            \* RESPOND TO JOIN REQUEST
            (***************************************************************)
            (* Any gate app receiving a join request will:                 *)
            (*   - respond with currently known joined apps,               *)
            (*   - pick new gossip friends,                                *)
            (*   - notify its gossip friends.                              *)
            (***************************************************************)
            if isGate[self] then
                await joinReqQueue[self] /= <<>>;
                with joinRequest = Head(joinReqQueue[self]),
                     updatedKnownApps = Append(knownApps, joinRequest.app)
                do
                    \* PULL FROM REQ QUEUE
                    joinReqQueue[self] := Tail(joinReqQueue[self]);
                    
                    \* RESPOND TO REQUESTER
                    joinRespQueue[joinRequest.app] := Append(
                        joinRespQueue[joinRequest.app], 
                        NewJoinRespMsg(self, SelectSeq(knownApps, 
                            LAMBDA app: app /= joinRequest.app)));
                    
                    \* UPDATE KNOWN APPS AND PICK NEW GOSSIP FRIENDS
                    if joinRequest.app \notin PT!Range(knownApps)
                    then
                        knownApps := updatedKnownApps;
                        gossipFriends := PickGossipFriends(self, updatedKnownApps);
                    end if;
                    
                    \* NOTIFY NETWORK OF NEW JOINER
                    notify(gossipFriends -- joinRequest.app, joinRequest.app);
                    
                    \* SET STATUS TO JOINED
                    joined := TRUE;
                end with;
            end if;
        or
            \* RECEIVE JOIN RESPONSE
            (***************************************************************)
            (* Upon receiving a join response from a gate app, an app      *)
            (* will have to update its known apps and pick gossip friends. *)
            (***************************************************************)
            await joinRespQueue[self] /= <<>>;
            with joinResponse = Head(joinRespQueue[self])
            do
                \* PULL FROM RESP QUEUE      
                joinRespQueue[self] := Tail(joinRespQueue[self]);
                
                \* UPDATE KNOWN APPS
                knownApps := MergeKnownApps(knownApps, joinResponse.knownHosts);
                
                \* PICK NEW GOSSIP FRIENDS
                gossipFriends := PickGossipFriends(self, joinResponse.knownHosts);
                
                \* SET STATUS TO JOINED
                joined := TRUE;
            end with;
        or
            \* RECEIVE JOIN NOTIFICATION
            (***************************************************************)
            (* Upon receiving a join notification from any other app,      *)
            (* an app updates its known apps as well as its gossips.       *)
            (***************************************************************)
            await newJoinerNotif[self] /= <<>>;
            with joinNotifMsg = Head(newJoinerNotif[self])
            do
                \* PULL FROM NOTIF QUEUE
                newJoinerNotif[self] := Tail(newJoinerNotif[self]);
                
                \* UPDATE KNOWN APPS
                knownApps := MergeKnownApps(knownApps, <<joinNotifMsg.app>>);
                
                \* PICK NEW GOSSIP FRIENDS
                gossipFriends := PickGossipFriends(self, knownApps);
            end with;
            
        (*******************************************************************)
        (* Following are the actions applying to the shopy-list            *)
        (* managed by the app.                                             *)
        (* We need to abstract actions down to one single add action       *)
        (* in order not to have infinite loops between adding, removing    *)
        (* and so on. User behavior must not be constrained because it     *)
        (* cannot be controlled.                                           *)
        (*******************************************************************)            
        or
            \* ADD
            await Cardinality(shopyList[self]) < Cardinality(PRODUCTS);
            shopyList[self] := shopyList[self] ++ NewShopyItem(shopyList[self]);
            
        (*******************************************************************)
        (* Below actions manage the synchronization of the list.           *)
        (*******************************************************************)
        or
            \* SEND SYNC REQUEST
            with a \in (PT!Range(knownApps) -- self),
                 req = NewSyncReq(self)
            do
                takenIDs := takenIDs ++ req.id;
                syncReqQueue[a] := Append(syncReqQueue[a], req);
            end with;
        or
            \* RCV SYNC REQUEST
            await syncReqQueue[self] /= <<>>;
            with syncRequest = Head(syncReqQueue[self]),
                 mergeResult = shopyList[self] \union syncRequest.list,
                 newResp = NewSyncResp(self, mergeResult, syncRequest.id) 
            do
                
                syncReqQueue[self] := Tail(syncReqQueue[self]);
                \* merge from request app
                shopyList[self] := mergeResult;
                syncRespQueue[syncRequest.app] := Append(syncRespQueue[syncRequest.app], newResp);
            end with;
        or
            \* RCV SYNC RESPONSE
            await syncRespQueue[self] /= <<>>;
            with syncResponse = Head(syncRespQueue[self]),
                 mergeResult = shopyList[self] \union syncResponse.list 
            do
                
                shopyList[self] := mergeResult;
                syncRespQueue[self] := Tail(syncRespQueue[self]);
            end with;
        end either;
    end while;
end process;

end algorithm;
*)
\* BEGIN TRANSLATION (chksum(pcal) = "ec56fe87" /\ chksum(tla) = "38265ee0")
VARIABLES isGate, shopyList, syncReqQueue, syncRespQueue, joinReqQueue, 
          joinRespQueue, newJoinerNotif, takenIDs

(* define statement *)
NewShopyItem(list) ==
    [id     |-> (CHOOSE x \in PRODUCTS: ~\E i \in list: x = i.id),
     bought |-> FALSE]

ExistingShopyItem(list) == CHOOSE x \in list: TRUE

ExistingNotBoughtShopyItem(list) == CHOOSE x \in list: x.bought = FALSE





NewSyncMsg(id, a, l, ml, t) ==
    [id |-> id,
     app |-> a,
     list |-> l,
     mergedList |-> ml,
     type |-> t]

NewSyncReqMsg(a, l, ml, t) ==
    NewSyncMsg(
        (CHOOSE i \in IDs: \A ti \in takenIDs: i /= ti),
        a, l, ml, t
    )

NewSyncReq(app) ==
    NewSyncReqMsg(app, shopyList[app], {}, REQ_SYNC_ACTION)

NewSyncResp(app, mergeResult, id) ==
    NewSyncMsg(id, app, shopyList[app], mergeResult, RESP_SYNC_ACTION)





NewJoinReqMsg(app) == [app |-> app]

NewJoinRespMsg(app, hosts) ==
    [app |-> app,
     knownHosts |-> hosts]

GateApps == {a \in APPS: isGate[a]}

NewJoinerNotifReq(app) == [app |-> app]




PickGossipFriends(app, knownApps) ==
    LET KnownApps ==
            PT!ReduceSeq(
                LAMBDA a, acc: IF a = app THEN acc ELSE Append(acc, a),
                knownApps, <<>>)

        Opposit ==
            PT!Index(KnownApps, app) + (Len(KnownApps) \div 2) - (Len(KnownApps) % 2)

        PreviousIndex(i) ==
            IF Len(KnownApps) < 3
            THEN 1
            ELSE IF i = 1 THEN Len(KnownApps) ELSE i - 1

        NextIndex(i) ==
            IF Len(KnownApps) < 3
            THEN Len(KnownApps)
            ELSE IF i = Len(KnownApps) THEN 1 ELSE i + 1

    IN {KnownApps[PreviousIndex(Opposit)],
        KnownApps[NextIndex(Opposit)]}





MergeKnownApps(apps1, apps2) ==
    LET AppSeq(n) == PT!SeqOf(APPS, n)

        Contains(appSeq, appItem) ==
            Cardinality(PT!Matching(appSeq, appItem)) > 0

        f[args \in AppSeq(Len(apps1))
                   \X AppSeq(Len(apps2))
                   \X AppSeq(Len(apps1) + Len(apps2))] ==

            LET l1 == args[1]

                l2 == args[2]

                acc == args[3]

                PickFromL1 == f[<<
                    Tail(l1),
                    l2,
                    Append(acc, Head(l1))>>]

                SkipOneL1 == f[<<
                    Tail(l1),
                    l2,
                    acc>>]

                PickFromL2 == f[<<
                    l1,
                    Tail(l2),
                    Append(acc, Head(l2))>>]

                SkipOneL2 == f[<<
                    l1,
                    Tail(l2),
                    acc>>]
            IN
                IF Len(l2) = 0
                THEN IF Len(l1) = 0
                     THEN acc
                     ELSE IF Contains(acc, Head(l1)) THEN SkipOneL1 ELSE PickFromL1
                ELSE IF Head(l1) /= Head(l2)
                     THEN IF Contains(acc, Head(l2)) THEN SkipOneL2 ELSE PickFromL2
                     ELSE IF Contains(acc, Head(l1)) THEN SkipOneL1 ELSE PickFromL1
    IN f[<<apps1, apps2, <<>>>>]

VARIABLES joined, gossipFriends, knownApps

vars == << isGate, shopyList, syncReqQueue, syncRespQueue, joinReqQueue, 
           joinRespQueue, newJoinerNotif, takenIDs, joined, gossipFriends, 
           knownApps >>

ProcSet == (APPS)

Init == (* Global variables *)
        /\ isGate = [a \in APPS |-> a \in GATEAPPS]
        /\ shopyList = [a \in APPS |-> {}]
        /\ syncReqQueue = [a \in APPS |-> <<>>]
        /\ syncRespQueue = [a \in APPS |-> <<>>]
        /\ joinReqQueue = [a \in APPS |-> <<>>]
        /\ joinRespQueue = [a \in APPS |-> <<>>]
        /\ newJoinerNotif = [a \in APPS |-> <<>>]
        /\ takenIDs = {}
        (* Process ClientApp *)
        /\ joined = [self \in APPS |-> FALSE]
        /\ gossipFriends = [self \in APPS |-> {}]
        /\ knownApps = [self \in APPS |-> <<self>>]

ClientApp(self) == /\ \/ /\ ~joined[self]
                         /\ \E a \in (GateApps -- self):
                              joinReqQueue' = [joinReqQueue EXCEPT ![a] = Append(joinReqQueue[a], NewJoinReqMsg(self))]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinRespQueue, newJoinerNotif, takenIDs, joined, gossipFriends, knownApps>>
                      \/ /\ IF isGate[self]
                               THEN /\ joinReqQueue[self] /= <<>>
                                    /\ LET joinRequest == Head(joinReqQueue[self]) IN
                                         LET updatedKnownApps == Append(knownApps[self], joinRequest.app) IN
                                           /\ joinReqQueue' = [joinReqQueue EXCEPT ![self] = Tail(joinReqQueue[self])]
                                           /\ joinRespQueue' = [joinRespQueue EXCEPT ![joinRequest.app] =                               Append(
                                                                                                          joinRespQueue[joinRequest.app],
                                                                                                          NewJoinRespMsg(self, SelectSeq(knownApps[self],
                                                                                                              LAMBDA app: app /= joinRequest.app)))]
                                           /\ IF joinRequest.app \notin PT!Range(knownApps[self])
                                                 THEN /\ knownApps' = [knownApps EXCEPT ![self] = updatedKnownApps]
                                                      /\ gossipFriends' = [gossipFriends EXCEPT ![self] = PickGossipFriends(self, updatedKnownApps)]
                                                 ELSE /\ TRUE
                                                      /\ UNCHANGED << gossipFriends, 
                                                                      knownApps >>
                                           /\ newJoinerNotif' =               [a \in APPS |->
                                                                IF a \in (gossipFriends'[self] -- joinRequest.app)
                                                                THEN Append(newJoinerNotif[a], NewJoinerNotifReq((joinRequest.app)))
                                                                ELSE newJoinerNotif[a]]
                                           /\ joined' = [joined EXCEPT ![self] = TRUE]
                               ELSE /\ TRUE
                                    /\ UNCHANGED << joinReqQueue, 
                                                    joinRespQueue, 
                                                    newJoinerNotif, joined, 
                                                    gossipFriends, knownApps >>
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, takenIDs>>
                      \/ /\ joinRespQueue[self] /= <<>>
                         /\ LET joinResponse == Head(joinRespQueue[self]) IN
                              /\ joinRespQueue' = [joinRespQueue EXCEPT ![self] = Tail(joinRespQueue[self])]
                              /\ knownApps' = [knownApps EXCEPT ![self] = MergeKnownApps(knownApps[self], joinResponse.knownHosts)]
                              /\ gossipFriends' = [gossipFriends EXCEPT ![self] = PickGossipFriends(self, joinResponse.knownHosts)]
                              /\ joined' = [joined EXCEPT ![self] = TRUE]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinReqQueue, newJoinerNotif, takenIDs>>
                      \/ /\ newJoinerNotif[self] /= <<>>
                         /\ LET joinNotifMsg == Head(newJoinerNotif[self]) IN
                              /\ newJoinerNotif' = [newJoinerNotif EXCEPT ![self] = Tail(newJoinerNotif[self])]
                              /\ knownApps' = [knownApps EXCEPT ![self] = MergeKnownApps(knownApps[self], <<joinNotifMsg.app>>)]
                              /\ gossipFriends' = [gossipFriends EXCEPT ![self] = PickGossipFriends(self, knownApps'[self])]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, takenIDs, joined>>
                      \/ /\ Cardinality(shopyList[self]) < Cardinality(PRODUCTS)
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] ++ NewShopyItem(shopyList[self])]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, newJoinerNotif, takenIDs, joined, gossipFriends, knownApps>>
                      \/ /\ \E a \in (PT!Range(knownApps[self]) -- self):
                              LET req == NewSyncReq(self) IN
                                /\ takenIDs' = takenIDs ++ req.id
                                /\ syncReqQueue' = [syncReqQueue EXCEPT ![a] = Append(syncReqQueue[a], req)]
                         /\ UNCHANGED <<shopyList, syncRespQueue, joinReqQueue, joinRespQueue, newJoinerNotif, joined, gossipFriends, knownApps>>
                      \/ /\ syncReqQueue[self] /= <<>>
                         /\ LET syncRequest == Head(syncReqQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncRequest.list IN
                                LET newResp == NewSyncResp(self, mergeResult, syncRequest.id) IN
                                  /\ syncReqQueue' = [syncReqQueue EXCEPT ![self] = Tail(syncReqQueue[self])]
                                  /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                  /\ syncRespQueue' = [syncRespQueue EXCEPT ![syncRequest.app] = Append(syncRespQueue[syncRequest.app], newResp)]
                         /\ UNCHANGED <<joinReqQueue, joinRespQueue, newJoinerNotif, takenIDs, joined, gossipFriends, knownApps>>
                      \/ /\ syncRespQueue[self] /= <<>>
                         /\ LET syncResponse == Head(syncRespQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncResponse.list IN
                                /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                /\ syncRespQueue' = [syncRespQueue EXCEPT ![self] = Tail(syncRespQueue[self])]
                         /\ UNCHANGED <<syncReqQueue, joinReqQueue, joinRespQueue, newJoinerNotif, takenIDs, joined, gossipFriends, knownApps>>
                   /\ UNCHANGED isGate

Next == (\E self \in APPS: ClientApp(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in APPS : SF_vars(ClientApp(self))

\* END TRANSLATION

(***************************************************************************)
(* There's no duplicated items in the sequence 'seq'.                      *)
(***************************************************************************)
NoDuplicates(seq) ==
    \A i, j \in DOMAIN seq:
        i /= j => seq[i] /= seq[j]

(***************************************************************************)
(* All apps in 'joinedApps' that joined the network.                       *)
(***************************************************************************)
JoinedApps(joinedF, apps) == {j \in apps: joinedF[j]}

(***************************************************************************)
(* The count of gossips for an app 'a' is the number of other joined apps  *)
(* that lists 'a' in its gossip friends set.                               *)
(***************************************************************************)
CountGossipOf(app, gossips, joinedApps) ==
    PT!ReduceSet(
        LAMBDA a, acc: acc + (IF app \in gossips[a] /\ joinedApps[a]
                              THEN 1 ELSE 0),
        APPS, 0)

(***************************************************************************)
(* The average of gossips is the average count over all joined apps        *)
(* of the count of gossips for an app.                                     *)
(***************************************************************************)
AverageGossipOf(gossips, joinedApps) ==
    PT!ReduceSet(
        LAMBDA a, acc: acc + CountGossipOf(a, gossips, joinedApps),
        APPS, 0)
    \div
    Cardinality(JoinedApps(joinedApps, APPS))

ExistsRoute(from, to, _gossipFriends) ==
    LET f[<<app, visited>> \in APPS \X SUBSET APPS] ==
            to \in _gossipFriends[app]
            \/ \E a \in (_gossipFriends[app] \ visited): f[a, visited ++ app]
    IN from = to \/ f[<<from, {}>>]

TypeOK ==
    /\ \A a \in APPS: 
        (*******************************************************************)
        (* Checking on variables' domains.                                 *)
        (*******************************************************************)
        /\ shopyList[a] \subseteq ShopyItems
        /\ PT!Range(syncReqQueue[a]) \subseteq SyncMsgs
        /\ PT!Range(syncRespQueue[a]) \subseteq SyncMsgs
        \* The queue for join requests is only for gate apps
        /\ IF isGate[a] 
           THEN PT!Range(joinReqQueue[a]) \subseteq JoinReqMsgs
                /\ \A req \in PT!Range(joinReqQueue[a]): req.app /= a
           ELSE joinReqQueue[a] = <<>>
        /\ PT!Range(joinRespQueue[a]) \subseteq JoinRespMsgs
        /\ PT!Range(newJoinerNotif[a]) \subseteq JoinNotifMsgs
        \* knownApps is a collection of unique, ordered apps.
        /\ PT!Range(knownApps[a]) \subseteq APPS
        /\ NoDuplicates(knownApps[a])
        \* Apps don't gossip themselves
        /\ gossipFriends[a] \subseteq (APPS -- a)
    /\ takenIDs \subseteq IDs

GossipInvariants ==
    /\ \A a \in APPS: 
        \* Invariant when we're connected or not.
        /\ gossipFriends[a] /= {}
           <=> knownApps[a] /= <<a>>
    (***********************************************************************)
    (* We verify that for every joined app, the count of any other joined  *)
    (* app gossiping the new joiner is more or less in the average.        *)
    (***********************************************************************)
    /\ \A ja \in JoinedApps(joined, APPS): CountGossipOf(ja, gossipFriends, joined) = 0 \/ (
           /\ CountGossipOf(ja, gossipFriends, joined) >= AverageGossipOf(gossipFriends, joined) - 1
           /\ CountGossipOf(ja, gossipFriends, joined) <= AverageGossipOf(gossipFriends, joined) + 1
       )
    
Liveness ==
    \* At some point, someone has joined and gossips have been assigned.
    /\ <>(\A ja \in JoinedApps(joined, APPS): joined[ja] /\ CountGossipOf(ja, gossipFriends, joined) > 0)
    
    /\ \A a \in APPS:
           \* Joining leads to having a route from every other joined app to the joiner.
           /\ joined[a]
              ~> \A a2 \in JoinedApps(joined, APPS -- a): 
                     ExistsRoute(a2, a, gossipFriends)
           \* Joining leads to every other joined app adding the new joiner to its list of known apps.
           /\ joined[a]
              ~> \A a2 \in JoinedApps(joined, APPS -- a):
                     a \in PT!Range(knownApps[a2])

=============================================================================
\* Modification History
\* Last modified Wed Mar 17 23:42:41 CET 2021 by davd
\* Created Tue Mar 02 12:33:43 CET 2021 by davd
