#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "gmap" @@ fun _ ->
  Ok [
    Pkg.mllib "src/gmap.mllib";
  ]
