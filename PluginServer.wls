#!/usr/bin/env wolframscript
(* ::Package:: *)

<<KirillBelov`Internal`
<<KirillBelov`Objects`
<<KirillBelov`CSockets`
<<KirillBelov`TCPServer`
<<KirillBelov`HTTPHandler`


server = CSocketOpen["0.0.0.0", 8080]; 
tcp = TCPServer[]; 
http = HTTPHandler[]; 
tcp += http; 
listener = SocketListen[server, tcp@Echo@#&]; 


http["MessageHandler", "Echo"] = Function[Echo[#Data, "REQUEST"]; False] -> Function[Null]; 


http["MessageHandler", "OPTIONS"] = 
Function[
	#Method === "OPTIONS" && 
	StringMatchQ[#Path, "/v1/chat/completions", IgnoreCase -> True]
] -> 
Function[
	<|
		"Headers" -> <|
			"Allow" -> "OPTONS, GET, HEAD, POST", 
			"Server" -> "WWS", 
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
	Echo[#];
	UsingFrontEnd[Echo[<|
		"Headers" -> <|
			"Connection" -> "keep-alive", 
            "Authorization" -> "Bearer N/A", 
            "Allow" -> "Origin", 
            "Access-Control-Allow-Origin" -> "*"
		|>, 
		"Body" -> getImage[StringSplit[StringTrim[#Path, "/"], "/"][[-1]]], 
		"StatusCode" -> 200, 
		"ContentType" -> "image/png"
	|>]]
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
	Echo[#, "REQUEST"]; 
	Echo[<|
		"Headers" -> <|
			"Connection" -> "keep-alive", 
            "Authorization" -> "Bearer N/A", 
            "Allow" -> "Origin", 
            "Access-Control-Allow-Origin" -> "*"
		|>, 
		"Body" -> ExportString[<|
			"id" -> CreateUUID["chatcmpl-"], 
			"created" -> UnixTime[], 
			"model" -> #Body["model"], 
			"object" -> "chat.completion", 
			"choices" -> {<|
				"index" -> 0, 
				"message" -> <|
					"role" -> "assistant", 
					"content" -> Echo[wolframAlphaMarkdown[#Body[["messages", -1, "content"]]]]
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
	|>, "RESPONSE"]
]; 


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
			$images[img] = Echo[pod["Content"]];
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
