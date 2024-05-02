#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`Objects`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`

server = CSocketOpen["localhost", 8080]; 
listener = SocketListen[server, func]; 


func[packet_] := (
	Print[packet["Data"]]; 
	Print[""]; 
	If[packet["Event"] === "Received", 
		WriteString[packet["SourceSocket"], 
			"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 4\r\nConnection: keep-alive\r\n\r\ntest"
		]; 
		Close[packet["SourceSocket"]]
	]
); 

While[True, Pause[0.1]]; 
