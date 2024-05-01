#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`Objects`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


func[packet_] := (Print[packet["Data"]]; Print["\n"]); 

server = CSocketOpen["localhost", 8080]; 
listener = SocketListen[server, (func[#]; tcp@#)&]; 
tcp = TCPServer[]; 
http = HTTPHandler[]; 
tcp += http; 
