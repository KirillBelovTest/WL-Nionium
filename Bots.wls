#!/usr/bin/env wolframscript
(* ::Package:: *)

$HistoryLength = 0; 
SetDirectory[NotebookDirectory[]]; 
Get["KirillBelov`WebSocketLink`"]; 
$settings = Import["Bots.json", "RawJSON"]; 


connection = WebSocketConnect["wss://chat.nionium.ai/websocket"]; 
connection["Deserializer"] = Function[m, ImportString[ExportString[m, "Text"], "RawJSON"]]; 
connection["Serializer"] = Function[m, ImportString[ExportString[m, "RawJSON"], "Text"]]; 
connection["EventHandler", "Echo"] = Function[{c, m}, Echo[m]]; 
connection["EventHandler", "Pong"] = Function[{c, m}, 
	If[m["msg"] === "ping", 
		WebSocketSend[c, <|"msg" -> "pong"|>]
	]
]; 
WebSocketSend[connection, <|"msg" -> "connect", "version" -> "1", "support" -> {"1"}|>]; 


WebSocketSend[connection, <|
	"msg" -> "method", 
	"method" -> "login", 
	"id" -> ToString[RandomInteger[100000]], 
	"params" -> {
		<|"resume" -> $settings[[1, "token"]]|>
	}
|>]; 


id := ToString[RandomInteger[10^9]]; 


WebSocketSend[connection, <|
	"msg" -> "sub", 
	"id" -> id, 
	"name" -> "stream-room-messages", 
	"params" -> {"YASNTcRdH58eQmKwg", False}
|>]; 


mid := StringJoin[RandomChoice[CharacterRange["a", "z"] ~ Join ~ CharacterRange["A", "Z"] ~ Join ~ CharacterRange["0", "9"], 17]]


messageFromUserQ[$m_Association] := 
Keys[$m] === {"msg", "collection", "id", "fields"} && 
$m[["msg"]] === "changed" && 
$m[["collection"]] === "stream-room-messages" && 
Length[$m[["fields", "args"]]] === 1 && 
Keys[$m[["fields", "args", 1]]] === {"_id", "rid", "msg", "ts", "u", "unread", "_updatedAt", "urls", "mentions", "channels", "md"} && 
Length[Select[$m[["fields", "args", 1, "mentions"]], #username === "chatgpt.bot"&]] === 1 && 
$m[["fields", "args", 1, "u", "username"]] =!= "chatgpt.bot"; 


trimMentions[msg_String, mentions_List] := 
StringTrim[StringReplace[msg, Map[WhitespaceCharacter... ~~ "@" <> #username ~~ WhitespaceCharacter... -> " "&, mentions]]]; 


SetAttributes[chatComplete, HoldFirst]


Options[chatComplete] = {
	"endpoint" -> "https://api.typingmind.com/api", 
	"temperature" -> 0.7, 
	"apiToken" -> $settings[[1, "apiToken"]], 
	"model" -> "gpt-4-turbo-preview"
}; 


chatComplete[chat_Symbol?AssociationQ, key_String, text_String, OptionsPattern[]] := 
Module[{
	endpoint = OptionValue["endpoint"], 
	t = OptionValue["temperature"], token = OptionValue["apiToken"], 
	model = OptionValue["model"], response, assoc, url
}, 
	If[!KeyExistsQ[chat, key], chat[key] = {}];

	Echo[endpoint, "AI[Endpoint]"]; 
	Echo[model, "AI[Model]"]; 

	url = StringTrim[endpoint, "/"] <> "/v1/chat/completions"; 
	response = URLRead[HTTPRequest[url, <|
		Method -> "POST", 
		"ContentType" -> "application/json", 
		"Headers" -> {
			"Authorization" -> "Bearer " <> token, 
			"X-API-KEY" -> token
		}, 
		"Body" -> ExportString[<|
			"model" -> model, 
			"messages" -> (chat[key] = Append[chat[key], <|"role" -> "user", "content" -> text|>]), 
			"temperature" -> t
		|>, "RawJSON", CharacterEncoding -> "UTF-8"]
	|>]]; 
	
	assoc = ImportString[ExportString[response["Body"], "Text"], "RawJSON"]; 
	
	chat[key] = Append[chat[key], assoc[["choices", 1, "message"]]]; 
	
	assoc[["choices", 1, "message", "content"]]
]


$chats = <||>; 


connection["EventHandler", "Mention"] = Function[{c, m}, 
	If[messageFromUserQ[m], 
		Module[{$m = m[["fields", "args", 1]]}, 
			
			If[!KeyExistsQ[$chats, $m["rid"]], 
				$chats[$m["rid"]] = {}
			];
			
			WebSocketSend[c, <|
				"msg" -> "method",
				"method" -> "sendMessage",
				"id" -> ToString[RandomInteger[1000000]],
				"params" -> {
					<|
						"_id" -> mid,
						"rid" -> $m["rid"],
						"msg" -> chatComplete[$chats, $m["rid"], trimMentions[$m["msg"], $m["mentions"]]]
					|>
				}
			|>]
		]
	]
]; 


While[True, Pause[0.01]]; 
