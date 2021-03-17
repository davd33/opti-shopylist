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
            (CHOOSE i \in IDs: \A ti \in takenIDs: i = ti),
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
    
    PickGossipFriends(app, knownApps) ==
        LET Opposit ==
                PT!Index(knownApps, app) + (Len(knownApps) \div 2) - (Len(knownApps) % 2)
                
            PreviousIndex(i) ==
                IF Len(knownApps) < 3
                THEN 1
                ELSE IF i = 1 THEN Len(knownApps) ELSE i - 1
           
            NextIndex(i) ==
                IF Len(knownApps) < 3
                THEN Len(knownApps)
                ELSE IF i = Len(knownApps) THEN 1 ELSE i + 1
        IN {knownApps[PreviousIndex(Opposit)], knownApps[NextIndex(Opposit)]}
    
    (***********************************************************************)
    (* Not used, it's an example of how we'd keep the ordering of          *)
    (* the responsed knownApps sequence on joining.                        *)
    (* If it's used, it should be with small sequences to not generate     *)
    (* enormous sets.                                                      *)
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

macro Notify(gossipFriends, newJoiner)
begin
    with a \in gossipFriends
    do
        newJoinerNotif[a] := Append(newJoinerNotif[a], NewJoinerNotifReq(app));
    end with;
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
fair process ClientApp \in APPS
variables
    joined = FALSE,
    gossipFriends = {},
    knownApps = <<self>>;

begin AppLoop:
    while TRUE do

        (*******************************************************************)
        (* Below are actions to manage the connection to the network.      *)
        (*******************************************************************)
        either
            \* SEND JOIN REQUEST
            with a \in (GateApps -- self)
            do
                joinReqQueue[a] := Append(joinReqQueue[a], NewJoinReqMsg(self));
            end with;
        or
            \* RESPOND TO JOIN REQUEST
            if isGate[self] then
                await joinReqQueue[self] /= <<>>;
                with joinRequest = Head(joinReqQueue[self]),
                     updatedKnownApps = Append(knownApps, joinRequest.app)
                do
                    
                    joinRespQueue[joinRequest.app] := Append(
                        joinRespQueue[joinRequest.app], 
                        NewJoinRespMsg(self, SelectSeq(knownApps, 
                            LAMBDA app: app /= joinRequest.app)));
                    
                    if joinRequest.app \notin PT!Range(knownApps)
                    then
                        knownApps := updatedKnownApps;
                        gossipFriends := PickGossipFriends(self, Tail(updatedKnownApps));
                    end if;
                        
                    joinReqQueue[self] := Tail(joinReqQueue[self]);
                    
                    joined := TRUE;
                end with;
            end if;
        or
            \* RECEIVE JOIN RESPONSE
            await joinRespQueue[self] /= <<>>;
            with joinResponse = Head(joinRespQueue[self]),
                 newKnownApps = PT!Range(joinResponse.knownHosts) \ PT!Range(knownApps)
            do
                gossipFriends := PickGossipFriends(self, joinResponse.knownHosts);
                
                knownApps := knownApps \o PT!OrderSet(newKnownApps);
                        
                joinRespQueue[self] := Tail(joinRespQueue[self]);
                
                joined := TRUE;
            end with;
            
        (*******************************************************************)
        (* Following are the actions applying to the shopy-list            *)
        (* managed by the app.                                             *)
        (*******************************************************************)            
        or
            \* ADD
            await Cardinality(shopyList[self]) < Cardinality(PRODUCTS);
            shopyList[self] := shopyList[self] ++ NewShopyItem(shopyList[self]);
        or
            \* REMOVE
            await shopyList[self] /= {};
            shopyList[self] := shopyList[self] -- ExistingShopyItem(shopyList[self]);
        or
            \* ITEM HAS BEEN BOUGHT
            await shopyList[self] /= {};
            await \E item \in shopyList[self]: ~item.bought;
            with modifiedItem = ExistingNotBoughtShopyItem(shopyList[self]) 
            do
                shopyList[self] := shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE];
            end with;
            
        (*******************************************************************)
        (* Below actions manage the synchronization of the list.           *)
        (*******************************************************************)
        or
            \* SEND SYNC REQUEST
            with a \in (PT!Range(knownApps) -- self) 
            do
                syncReqQueue[a] := Append(syncReqQueue[a], NewSyncReq(self));
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
\* BEGIN TRANSLATION (chksum(pcal) = "a4dd4f8d" /\ chksum(tla) = "e6dbda73")
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
        (CHOOSE i \in IDs: \A ti \in takenIDs: i = ti),
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
    LET Opposit ==
            PT!Index(knownApps, app) + (Len(knownApps) \div 2) - (Len(knownApps) % 2)

        PreviousIndex(i) ==
            IF Len(knownApps) < 3
            THEN 1
            ELSE IF i = 1 THEN Len(knownApps) ELSE i - 1

        NextIndex(i) ==
            IF Len(knownApps) < 3
            THEN Len(knownApps)
            ELSE IF i = Len(knownApps) THEN 1 ELSE i + 1
    IN {knownApps[PreviousIndex(Opposit)], knownApps[NextIndex(Opposit)]}







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

ClientApp(self) == /\ \/ /\ \E a \in (GateApps -- self):
                              joinReqQueue' = [joinReqQueue EXCEPT ![a] = Append(joinReqQueue[a], NewJoinReqMsg(self))]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ IF isGate[self]
                               THEN /\ joinReqQueue[self] /= <<>>
                                    /\ LET joinRequest == Head(joinReqQueue[self]) IN
                                         LET updatedKnownApps == Append(knownApps[self], joinRequest.app) IN
                                           /\ joinRespQueue' = [joinRespQueue EXCEPT ![joinRequest.app] =                               Append(
                                                                                                          joinRespQueue[joinRequest.app],
                                                                                                          NewJoinRespMsg(self, SelectSeq(knownApps[self],
                                                                                                              LAMBDA app: app /= joinRequest.app)))]
                                           /\ IF joinRequest.app \notin PT!Range(knownApps[self])
                                                 THEN /\ knownApps' = [knownApps EXCEPT ![self] = updatedKnownApps]
                                                      /\ gossipFriends' = [gossipFriends EXCEPT ![self] = PickGossipFriends(self, Tail(updatedKnownApps))]
                                                 ELSE /\ TRUE
                                                      /\ UNCHANGED << gossipFriends, 
                                                                      knownApps >>
                                           /\ joinReqQueue' = [joinReqQueue EXCEPT ![self] = Tail(joinReqQueue[self])]
                                           /\ joined' = [joined EXCEPT ![self] = TRUE]
                               ELSE /\ TRUE
                                    /\ UNCHANGED << joinReqQueue, 
                                                    joinRespQueue, joined, 
                                                    gossipFriends, knownApps >>
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue>>
                      \/ /\ joinRespQueue[self] /= <<>>
                         /\ LET joinResponse == Head(joinRespQueue[self]) IN
                              LET newKnownApps == PT!Range(joinResponse.knownHosts) \ PT!Range(knownApps[self]) IN
                                /\ gossipFriends' = [gossipFriends EXCEPT ![self] = PickGossipFriends(self, joinResponse.knownHosts)]
                                /\ knownApps' = [knownApps EXCEPT ![self] = knownApps[self] \o PT!OrderSet(newKnownApps)]
                                /\ joinRespQueue' = [joinRespQueue EXCEPT ![self] = Tail(joinRespQueue[self])]
                                /\ joined' = [joined EXCEPT ![self] = TRUE]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinReqQueue>>
                      \/ /\ Cardinality(shopyList[self]) < Cardinality(PRODUCTS)
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] ++ NewShopyItem(shopyList[self])]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ shopyList[self] /= {}
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- ExistingShopyItem(shopyList[self])]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ shopyList[self] /= {}
                         /\ \E item \in shopyList[self]: ~item.bought
                         /\ LET modifiedItem == ExistingNotBoughtShopyItem(shopyList[self]) IN
                              shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE]]
                         /\ UNCHANGED <<syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ \E a \in (PT!Range(knownApps[self]) -- self):
                              syncReqQueue' = [syncReqQueue EXCEPT ![a] = Append(syncReqQueue[a], NewSyncReq(self))]
                         /\ UNCHANGED <<shopyList, syncRespQueue, joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ syncReqQueue[self] /= <<>>
                         /\ LET syncRequest == Head(syncReqQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncRequest.list IN
                                LET newResp == NewSyncResp(self, mergeResult, syncRequest.id) IN
                                  /\ syncReqQueue' = [syncReqQueue EXCEPT ![self] = Tail(syncReqQueue[self])]
                                  /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                  /\ syncRespQueue' = [syncRespQueue EXCEPT ![syncRequest.app] = Append(syncRespQueue[syncRequest.app], newResp)]
                         /\ UNCHANGED <<joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                      \/ /\ syncRespQueue[self] /= <<>>
                         /\ LET syncResponse == Head(syncRespQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncResponse.list IN
                                /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                /\ syncRespQueue' = [syncRespQueue EXCEPT ![self] = Tail(syncRespQueue[self])]
                         /\ UNCHANGED <<syncReqQueue, joinReqQueue, joinRespQueue, joined, gossipFriends, knownApps>>
                   /\ UNCHANGED << isGate, newJoinerNotif, takenIDs >>

Next == (\E self \in APPS: ClientApp(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in APPS : WF_vars(ClientApp(self))

\* END TRANSLATION

NoDuplicates(seq) ==
    \A i, j \in DOMAIN seq:
        i /= j => seq[i] /= seq[j]

JoinedApps(joinedApps) == {j \in APPS: joinedApps[j]}

CountGossipOf(app, gossips, joinedApps) ==
    PT!ReduceSet(
        LAMBDA a, acc: acc + (IF app \in gossips[a] /\ joinedApps[a]
                              THEN 1 ELSE 0),
        APPS, 0)

AverageGossipOf(gossips, joinedApps) ==
    PT!ReduceSet(
        LAMBDA a, acc: acc + CountGossipOf(a, gossips, joinedApps),
        APPS, 0)
    \div
    Cardinality(JoinedApps(joinedApps))

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
        \* Invariant when we're connected or not.
        /\ gossipFriends[a] /= {}
           <=> knownApps[a] /= <<a>>
        (*******************************************************************)
        (* Debug breakpoint for all apps 'a'.                              *)
        (*******************************************************************)
\*        \* a sync response has been sent
\*        /\ (syncRespQueue[a] = <<>>
\*            \* no shopy lists are empty 
\*            \/ shopyList[a] = {})
    /\ \A ja \in JoinedApps(joined): CountGossipOf(ja, gossipFriends, joined) = 0 \/ (
           /\ CountGossipOf(ja, gossipFriends, joined) >= AverageGossipOf(gossipFriends, joined) - 1
           /\ CountGossipOf(ja, gossipFriends, joined) <= AverageGossipOf(gossipFriends, joined) + 1
       )
    /\ takenIDs \subseteq IDs
    
Liveness ==
    \* At some point, someone has joined and gossips have been assigned.
    /\ <>(\A ja \in JoinedApps(joined): joined[ja] /\ CountGossipOf(ja, gossipFriends, joined) > 0)
    \* There's a route from every other connected app to a joined app.
    /\ \A a \in APPS:
           joined[a]
           ~> \A a2 \in {j \in (APPS -- a): joined[j]}: 
                  ExistsRoute(a2, a, gossipFriends)

=============================================================================
\* Modification History
\* Last modified Wed Mar 17 13:38:18 CET 2021 by davd
\* Created Tue Mar 02 12:33:43 CET 2021 by davd
