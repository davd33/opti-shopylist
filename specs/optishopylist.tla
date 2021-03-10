--------------------------- MODULE optishopylist ---------------------------

EXTENDS TLC, Integers, FiniteSets, Sequences

CONSTANTS PRODUCTS, APPS, IDs
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
     knownHosts: SUBSET APPS]

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
    isGate = [a \in APPS |-> FALSE],
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
    gossipFriends = {},
    knownApps = {};

begin AppLoop:
    while TRUE do
        
        (*******************************************************************)
        (* The 2 actions below set the current app instance                *)
        (* to be a Gate to the network or not to be.                       *)
        (*******************************************************************)
        either
            \* BECOME A GATE
            isGate[self] := TRUE;
        or
            \* STOP BEING A GATE
            isGate[self] := FALSE;
            
        (*******************************************************************)
        (* Below are actions to manage the connection to the network.      *)
        (*******************************************************************)
        or
            \* SEND JOIN REQUEST
            with a \in GateApps 
            do
                joinReqQueue[a] := Append(joinReqQueue[a], NewJoinReqMsg(self));
            end with;
        or
            \* RCV JOIN REQUEST
            if isGate[self] then
                await joinReqQueue[self] /= <<>>;
                with joinRequest = Head(joinReqQueue[self])
                do
                    
                    knownApps := knownApps ++ joinRequest.app;
                    
                    joinRespQueue[joinRequest.app] := Append(
                        joinRespQueue[joinRequest.app], 
                        NewJoinRespMsg(self, knownApps));
                        
                    joinReqQueue[self] := Tail(joinReqQueue[self]);
                end with;
            end if;
        or
            \* RCV JOIN RESPONSE
            await joinRespQueue[self] /= <<>>;
            with joinResponse = Head(joinRespQueue[self]) 
            do
                gossipFriends := joinResponse.assignedGossipFriends;
                knownApps := joinResponse.knownHosts;
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
            with a \in (knownApps -- self) 
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
\* BEGIN TRANSLATION (chksum(pcal) = "e7becbeb" /\ chksum(tla) = "11cc3074")
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

VARIABLES gossipFriends, knownApps

vars == << isGate, shopyList, syncReqQueue, syncRespQueue, joinReqQueue, 
           joinRespQueue, newJoinerNotif, takenIDs, gossipFriends, knownApps
        >>

ProcSet == (APPS)

Init == (* Global variables *)
        /\ isGate = [a \in APPS |-> FALSE]
        /\ shopyList = [a \in APPS |-> {}]
        /\ syncReqQueue = [a \in APPS |-> <<>>]
        /\ syncRespQueue = [a \in APPS |-> <<>>]
        /\ joinReqQueue = [a \in APPS |-> <<>>]
        /\ joinRespQueue = [a \in APPS |-> <<>>]
        /\ newJoinerNotif = [a \in APPS |-> <<>>]
        /\ takenIDs = {}
        (* Process ClientApp *)
        /\ gossipFriends = [self \in APPS |-> {}]
        /\ knownApps = [self \in APPS |-> {}]

ClientApp(self) == /\ \/ /\ isGate' = [isGate EXCEPT ![self] = TRUE]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ isGate' = [isGate EXCEPT ![self] = FALSE]
                         /\ UNCHANGED <<shopyList, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ \E a \in GateApps:
                              joinReqQueue' = [joinReqQueue EXCEPT ![a] = Append(joinReqQueue[a], NewJoinReqMsg(self))]
                         /\ UNCHANGED <<isGate, shopyList, syncReqQueue, syncRespQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ IF isGate[self]
                               THEN /\ joinReqQueue[self] /= <<>>
                                    /\ LET joinRequest == Head(joinReqQueue[self]) IN
                                         /\ knownApps' = [knownApps EXCEPT ![self] = knownApps[self] ++ joinRequest.app]
                                         /\ joinRespQueue' = [joinRespQueue EXCEPT ![joinRequest.app] =                               Append(
                                                                                                        joinRespQueue[joinRequest.app],
                                                                                                        NewJoinRespMsg(self, knownApps'[self]))]
                                         /\ joinReqQueue' = [joinReqQueue EXCEPT ![self] = Tail(joinReqQueue[self])]
                               ELSE /\ TRUE
                                    /\ UNCHANGED << joinReqQueue, 
                                                    joinRespQueue, knownApps >>
                         /\ UNCHANGED <<isGate, shopyList, syncReqQueue, syncRespQueue, gossipFriends>>
                      \/ /\ joinRespQueue[self] /= <<>>
                         /\ LET joinResponse == Head(joinRespQueue[self]) IN
                              /\ gossipFriends' = [gossipFriends EXCEPT ![self] = joinResponse.assignedGossipFriends]
                              /\ knownApps' = [knownApps EXCEPT ![self] = joinResponse.knownHosts]
                         /\ UNCHANGED <<isGate, shopyList, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue>>
                      \/ /\ Cardinality(shopyList[self]) < Cardinality(PRODUCTS)
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] ++ NewShopyItem(shopyList[self])]
                         /\ UNCHANGED <<isGate, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ shopyList[self] /= {}
                         /\ shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- ExistingShopyItem(shopyList[self])]
                         /\ UNCHANGED <<isGate, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ shopyList[self] /= {}
                         /\ \E item \in shopyList[self]: ~item.bought
                         /\ LET modifiedItem == ExistingNotBoughtShopyItem(shopyList[self]) IN
                              shopyList' = [shopyList EXCEPT ![self] = shopyList[self] -- modifiedItem ++ [modifiedItem EXCEPT !.bought = TRUE]]
                         /\ UNCHANGED <<isGate, syncReqQueue, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ \E a \in (knownApps[self] -- self):
                              syncReqQueue' = [syncReqQueue EXCEPT ![a] = Append(syncReqQueue[a], NewSyncReq(self))]
                         /\ UNCHANGED <<isGate, shopyList, syncRespQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ syncReqQueue[self] /= <<>>
                         /\ LET syncRequest == Head(syncReqQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncRequest.list IN
                                LET newResp == NewSyncResp(self, mergeResult, syncRequest.id) IN
                                  /\ syncReqQueue' = [syncReqQueue EXCEPT ![self] = Tail(syncReqQueue[self])]
                                  /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                  /\ syncRespQueue' = [syncRespQueue EXCEPT ![syncRequest.app] = Append(syncRespQueue[syncRequest.app], newResp)]
                         /\ UNCHANGED <<isGate, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                      \/ /\ syncRespQueue[self] /= <<>>
                         /\ LET syncResponse == Head(syncRespQueue[self]) IN
                              LET mergeResult == shopyList[self] \union syncResponse.list IN
                                /\ shopyList' = [shopyList EXCEPT ![self] = mergeResult]
                                /\ syncRespQueue' = [syncRespQueue EXCEPT ![self] = Tail(syncRespQueue[self])]
                         /\ UNCHANGED <<isGate, syncReqQueue, joinReqQueue, joinRespQueue, gossipFriends, knownApps>>
                   /\ UNCHANGED << newJoinerNotif, takenIDs >>

Next == (\E self \in APPS: ClientApp(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in APPS : WF_vars(ClientApp(self))

\* END TRANSLATION

CountNumberOfGossips(app) ==
    PT!ReduceSet(
        LAMBDA a, acc: acc + (IF app \in gossipFriends[a]
                              THEN 1 ELSE 0),
        APPS,
        0)

(***************************************************************************)
(* This invariant states that on each state we have                        *)
(* every app in the system be the gossip friend of                         *)
(* N+1 or N-1 other apps; N being the number of gossip                     *)
(* friends each app is assigned to.                                        *)
(***************************************************************************)
NumberOfGossipsBalanced(app) ==
    LET N == 2
    IN \/ CountNumberOfGossips(app) = N
       \/ CountNumberOfGossips(app) = N + 1
       \/ CountNumberOfGossips(app) = N - 1

TypeOK ==
    /\ \A a \in APPS: 
        /\ shopyList[a] \subseteq ShopyItems
        /\ PT!Range(syncReqQueue[a]) \subseteq SyncMsgs
        /\ PT!Range(syncRespQueue[a]) \subseteq SyncMsgs
        /\ IF isGate[a]
            THEN PT!Range(joinReqQueue[a]) \subseteq JoinReqMsgs
            ELSE TRUE
        /\ PT!Range(joinRespQueue[a]) \subseteq JoinRespMsgs
        /\ PT!Range(newJoinerNotif[a]) \subseteq JoinNotifMsgs
        /\ knownApps[a] \subseteq APPS
        /\ gossipFriends[a] \subseteq APPS
        /\ \/ /\ gossipFriends[a] /= {}
              /\ knownApps[a] /= {}
              /\ a \notin gossipFriends[a]
              /\ a \in knownApps[a]
           \/ /\ gossipFriends[a] = {}
              /\ knownApps[a] = {}
\*        /\ NumberOfGossipsBalanced(a)
    /\ takenIDs \subseteq IDs

=============================================================================
\* Modification History
\* Last modified Wed Mar 10 08:24:05 CET 2021 by davd
\* Created Tue Mar 02 12:33:43 CET 2021 by davd
