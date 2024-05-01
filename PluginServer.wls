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
tcp["CompleteHandler", "HTTP"] = httpQ -> HTTPPacketLength;
tcp["MessageHandler", "HTTP"] = httpQ -> http; 


func = (
	Echo[HTTPPacketQ[#], "HTTPPacketQ"]; 
	Echo[#Data, "REQUEST"]; 
)&; 


http["MessageHandler", "GET"] = 
AssocMatchQ[<|"Method" -> "GET", "Path" -> "/"|>] -> 
Function[Echo[<|
	"Body" -> "hello"
|>]]; 


httpQ[packet_?AssociationQ] := 
StringContainsQ[packet["Data"], "\r\n\r\n"]; 
