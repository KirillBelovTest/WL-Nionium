#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`Objects`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


server = CSocketOpen[8080]; 
listener = SocketListen[server, (func[#]; tcp@#)&]; 
tcp = TCPServer[]; 
http = HTTPHandler[]; 
tcp += http; 


While[True, Pause[0.1]]; 
