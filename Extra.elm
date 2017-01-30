module Extra exposing 
  ( asciiLetters, digits, punctuation, isAlpha, isNumeric, isAlphaNumeric 
  , head, tail, containsAny, excludes, excludesAll
  , capitalize, title
  )

import String
import Char


asciiLetters : String
asciiLetters =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

punctuation : String
punctuation =
  "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~"

digits : String
digits =
  "0123456789"

isAlpha : String -> Bool
isAlpha str =
  isOnlyOf str asciiLetters

isNumeric : String -> Bool
isNumeric str =
  isOnlyOf str digits

isAlphaNumeric : String -> Bool
isAlphaNumeric str =
  isOnlyOf str (asciiLetters ++ digits)

head : String -> String
head string =
  String.left 1 string

tail : String -> String
tail string =
  String.dropLeft 1 string

containsAny : String -> String -> Bool
containsAny sub str =
  case (String.uncons sub) of
    Just (head, tail) ->
      case String.contains (String.fromChar head) str of
        False -> containsAny tail str
        True -> True
    Nothing ->
      False

excludes : String -> String -> Bool
excludes sub str =
  String.contains sub str
    |> not

excludesAll : String -> String -> Bool
excludesAll sub str =
  containsAny sub str
    |> not

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

isOnlyOf : String -> String -> Bool
isOnlyOf str charset =
  case (String.uncons str) of
    Just (head, tail) ->
      case String.contains (String.fromChar head) charset of
        True -> isOnlyOf tail charset
        False -> False
    Nothing ->
      True
