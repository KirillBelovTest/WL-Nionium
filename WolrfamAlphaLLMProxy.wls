#!/usr/bin/env wolframscript
(* ::Package:: *)

Map[PacletSiteUpdate] @ PacletSites[]; 


PacletInstall["KirillBelov/CSockets"]; 
PacletInstall["KirillBelov/Internal"]; 
PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/HTTPHandler"]; 


<<KirillBelov`CSockets`
<<KirillBelov`Objects`
<<KirillBelov`Internal`
<<KirillBelov`HTTPHandler`


server = CSocketOpen["localhost", 8080]; 


http = HTTPHandler[]; 


handler = CSocketHandler[
	"Accumulator" -> <|"HTTP" -> HTTPPacketQ -> HTTPPacketLength|>, 
	"Handler" -> <|"HTTP" -> HTTPPacketQ -> http|>
]; 


listener = SocketListen[server, handler]; 


http["DefaultMessageHandler"] = Function[Echo[#]; 
	<|"Body" -> "404", "Code" -> 404|>
]; 


http["MessageHandler", "GETAlphaJSON"] = 
Function[
	#Method === "GET" && 
	StringMatchQ[#Path, "/api/v1/wolframAlphaJSON", IgnoreCase -> True]
] -> 
Function[
	<|
		"Headers" -> <|
			"Connection" -> "keep-alive", 
            "Authorization" -> "Bearer N/A", 
            "Allow" -> "Origin", 
            "Access-Control-Allow-Origin" -> "*"
		|>, 
		"Body" -> ExportByteArray[<|
			"result" -> wolframAlphaMarkdown[Echo[#Query["query"]]]
		|>, "RawJSON"], 
		"StatusCode" -> 200, 
		"ContentType" -> "application/json"
	|>
]; 


http["MessageHandler", "Health"] = 
Function[
	StringMatchQ[#Path, "/health", IgnoreCase -> True]
] -> 
Function[
	<|
		"Headers" -> <|
			"Connection" -> "keep-alive", 
            "Authorization" -> "Bearer N/A", 
            "Allow" -> "Origin", 
            "Access-Control-Allow-Origin" -> "*"
		|>, 
		"Body" -> "ok", 
		"StatusCode" -> 200, 
		"ContentType" -> "application/json"
	|>
]; 


wolframAlphaMarkdown[query_String] := 
ImportString[ExportString[
	URLExecute[StringReplace[WolframAlpha[query, "URL"], 
		"/v1/query.jsp" -> "/v1/llm-api"]], 
		"Table", 
		CharacterEncoding -> "ASCII"
	], 
	"String"
]; 


Echo["Plugin started"]; 


If[$FrontEnd === Null, While[True, Pause[0.01]]]; 
