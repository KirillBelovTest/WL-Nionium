#!/usr/bin/env wolframscript
(* ::Package:: *)

Map[PacletSiteUpdate, PacletSites[]]; 

PacletInstall["KirillBelov/Internal"]; 
PacletInstall["KirillBelov/Objects"]; 
PacletInstall["KirillBelov/CSockets"]; 
PacletInstall["KirillBelov/TCPServer"]; 
PacletInstall["KirillBelov/HTTPHandler"]; 

<<KirillBelov`Internal`
<<KirillBelov`Objects`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


server = CSocketOpen["localhost", 8080]; 
tcp = TCPServer[]; 
http = HTTPHandler[]; 
tcp += http; 
listener = SocketListen[server, tcp@#&]; 


printRequest[request_Association?AssociationQ] := (
	Print["= = = REQUEST = = ="]; 
	Print[request["Method"], " ", request["Path"]]; 
	KeyValueMap[Print[#1, ": ", #2]&] @ request["Headers"]; 
	Print[]; 
	Print[request["Body"]]; 
	Print["= = = = = = = = = ="]; 
	Print[""]; 
); 


http["MessageHandler", "PrintRequest"] = Function[printRequest[#]; False] -> Function[Null]; 


$icon = ExportString[
	Graphics[{Blue, Circle[]}, ImageSize -> 100], 
	"SVG"
]; 


http["MessageHandler", "Icon"] = 
AssocMatchQ[<|"Path" -> "/favicon.ico"|>] -> 
Function[<|"Body" -> $icon, "ContentType" -> "image/svg+xml"|>]; 


http["MessageHandler", "Health"] = 
AssocMatchQ[<|"Path" -> "/health"|>] -> 
Function[<|"Body" -> DateString[]|>]; 


http["MessageHandler", "OPTIONS"] = 
Function[
	#Method === "OPTIONS" && 
	StringMatchQ[#Path, "/v1/chat/completions", IgnoreCase -> True]
] -> 
Function[
	<|
		"Headers" -> <|
			"Allow" -> "OPTIONS, GET, POST", 
			"Server" -> "WWS", 
			"Connection" -> "keep-alive", 
			"Accept" -> "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8", 
			"Access-Control-Allow-Origin" -> #Headers["Origin"], 
			"Access-Control-Allow-Methods" -> "POST, GET, OPTIONS", 
			"Access-Control-Allow-Headers" -> #Headers["Access-Control-Request-Headers"], 
			"Content-Length" -> ToString[0]
		|>, 
		"Body" -> "", 
		"StatusCode" -> 204, 
		"ContentType" -> "text/plain"
	|>
]; 


http["MessageHandler", "Images"] = 
Function[
	#Method === "GET" && 
	StringMatchQ[#Path, "/images/" ~~ __, IgnoreCase -> True]
] -> 
Function[
	UsingFrontEnd[<|
		"Headers" -> <|
			"Connection" -> "keep-alive", 
            "Authorization" -> "Bearer N/A", 
            "Allow" -> "Origin", 
            "Access-Control-Allow-Origin" -> "*"
		|>, 
		"Body" -> getImage[StringSplit[StringTrim[#Path, "/"], "/"][[-1]]], 
		"StatusCode" -> 200, 
		"ContentType" -> "image/png"
	|>]
]; 


img404 := img404 = ExportByteArray[Style["404", "Title"], "PNG"]; 


$images = <||>; 


getImage[key_String] := 
If[KeyExistsQ[$images, key], 
	If[Head[$images[key]] =!= ByteArray, 
		$images[key] = ExportByteArray[Rasterize[$images[key], Background->None], "PNG", Background -> None], 
		$images[key]
	], 
	img404
]


http["MessageHandler", "POST"] = 
Function[
	#Method === "POST" && 
	StringMatchQ[#Path, "/v1/chat/completions", IgnoreCase -> True]
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
			"id" -> CreateUUID["chatcmpl-"], 
			"created" -> UnixTime[], 
			"model" -> #Body["model"], 
			"object" -> "chat.completion", 
			"choices" -> {<|
				"index" -> 0, 
				"message" -> <|
					"role" -> "assistant", 
					"content" -> wolframAlphaMarkdown[#Body[["messages", -1, "content"]]]
				|>, 
				"logprobs" -> Null
			|>}, 
			"usage" -> <|
				"prompt_tokens" -> 1, 
				"compltion_tokens" -> 1, 
				"total_tokens" -> 2
			|>
		|>, "RawJSON"], 
		"StatusCode" -> 200, 
		"ContentType" -> "application/json"
	|>
]; 


tcp["DefaultMessageHandler"] = Function[If[#Event === "Received", 
	Module[{json = ImportString[#Data, "RawJSON"]}, 
		ExportString[HTTPResponse[ExportString[<|
			"Headers" -> <|
				"Connection" -> "keep-alive", 
				"Authorization" -> "Bearer N/A", 
				"Allow" -> "Origin", 
				"Access-Control-Allow-Origin" -> "*"
			|>, 
			"Body" -> ExportByteArray[<|
				"id" -> CreateUUID["chatcmpl-"], 
				"created" -> UnixTime[], 
				"model" -> json["model"], 
				"object" -> "chat.completion", 
				"choices" -> {<|
					"index" -> 0, 
					"message" -> <|
						"role" -> "assistant", 
						"content" -> wolframAlphaMarkdown[json[["messages", -1, "content"]]]
					|>, 
					"logprobs" -> Null
				|>}, 
				"usage" -> <|
					"prompt_tokens" -> 1, 
					"compltion_tokens" -> 1, 
					"total_tokens" -> 2
				|>
			|>, "RawJSON"], 
			"StatusCode" -> 200, 
			"ContentType" -> "application/json"
		|>, "RawJSON"], <|"ContentType" -> "appliaction/json"|>], "HTTPResponse"]
	]
]]; 


wolframAlphaMarkdown[query_String] := 
UsingFrontEnd @ Module[{pods}, 
	pods = Association @ 
		KeyValueMap[#1 -> 
			Prepend[Association @ KeyValueMap[Function[{k, v}, k[[2]] -> v], #2], "Key" -> #1]&
		] @ 
		Map[Association] @ 
		GroupBy[First @* First] @ 
		If[LanguageIdentify[query] =!= Entity["Language", "English"], 
			WolframAlpha[Check[TextTranslation[query, "English"], query], "PodInformation"], 
			WolframAlpha[query, "PodInformation"]
		]; 
	
	StringTrim[StringRiffle[Values @ Map[podToMD] @ pods, "\n\n"]]
]; 


podToMD[___] := ""; 


podToMD[pod_Association?AssociationQ] := 
UsingFrontEnd @ Module[{result = "", img, content}, 
	If[KeyExistsQ[pod, "Title"], result = result <> "## " <> ToString[pod["Title"]] <> "\n\n"]; 
	If[pod["Key"][[-1]] =!= 0, 
		If[
			pod["Key"][[1]] === "Input" && 
			KeyExistsQ[pod, "Input"] && 
			Head[pod["Input"]] === HoldComplete, 
				result = result <> "Input code:  \n```wolfram\n" <> Apply[ToString, pod["Input"]] <> "\n```\n\n"
		]; 
		If[
			KeyExistsQ[pod, "Content"] && 
			Head[pod["Content"]] === RawBoxes, 
			result = result <> FrontEndExecute[FrontEnd`ExportPacket[Cell[pod["Content"]], "PlainText"]][[1]] <> "\n\n"
		]; 
		If[KeyExistsQ[pod, "DataFormats"] && pod["DataFormats"] === {"ComputableData", "FormattedData"}, 
			img = ToString[Hash[pod["Content"]]]; 
			$images[img] = pod["Content"];
			result = result <> "![](http://localhost:8080/images/" <> img <> ")\n\n"
		]; 
		If[KeyExistsQ[pod, "DataFormats"] && MemberQ[pod["DataFormats"], "TimeSeriesData"], 
			content = contentToTimeSeries[pod["Content"]]; 
			img = ToString[Hash[content]]; 
			$images[img] = content;
			result = result <> "![](http://localhost:8080/images/" <> img <> ")\n\n"
		]; 
	]; 
	result
]


contentToTimeSeries[content_RawBoxes] := 
FirstCase[ToExpression[FrontEndExecute[FrontEnd`ExportPacket[Cell[content], "InputText"]][[1]]], _Graphics, "", Infinity]


While[True, Pause[0.1]]; 
