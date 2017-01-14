module Extra exposing (..)

import String
import Char

head : String -> String
head string =
  String.left 1 string

tail : String -> String
tail string =
  String.dropLeft 1 string

capitalize : String -> String
capitalize string =
  let
    res =
      String.uncons string
  in
    case res of
      Nothing ->
        ""
      Just (head, tail) ->
        String.cons (Char.toUpper head) tail

title : String -> String
title string =
  String.split " " string
    |> List.map capitalize
    |> String.join " "

