

#load "Common.fs"
#load "Blob.fs"
#load "FInfo.fs"
#load "Action.fs"
#load "InteractiveUtils.fs"
#load "CommandLine.fs"

open Common
open Blob
open FInfo
open Action
open InteractiveUtils
open System.IO
open CommandLine

let u = UPath.fromUit
// fsharplint:disable Hints

let repo = { Path = DirectoryInfo "./testdata_work" }


let di = DirectoryInfo "./testdata_work"
di.Parent

di.Parent.Parent.Parent.Parent // /Users
di.Parent.Parent.Parent.Parent.Parent // /
di.Parent.Parent.Parent.Parent.Parent.Parent //  nulll

di.Parent.Parent.Parent.Parent.Parent.Parent = null

di.EnumerateDirectories(".uit")

Seq.toList( di.Parent.EnumerateDirectories(".uit") ) = []




findTopUitDir di.Parent



//
// Init
//

initRecursive repo



let dups = listDupMB repo
uniqIt repo dups.Head
listDupMB repo

//
// remove test
//
let remove = Remove.remove

let mb = lsmb repo (u "imgs/美子ちゃん.pxv")

let mb2 = remove repo mb (u "sns/美子ちゃん.pxv")

remove repo mb2 (u "imgs/美子ちゃん.pxv")

let mb3 = lshmb repo "2b0b5"


Remove.removeTrash repo mb3

lsmb repo (u ".uit/trash/美子ちゃん.pxv")

//
// toInstance test.



lsmb repo (u "sns/美子ちゃん.pxv")
lsa repo (u "imgs/美子ちゃん.pxv")
let mb4 = lshmb repo "2b0b5"

DInfo.findFInfo repo (u "imgs/美子ちゃん.pxv")
DInfo.findFInfo repo (u "sns/美子ちゃん.pxv")

toInstance repo mb4 (u "imgs/美子ちゃん.pxv.uitlnk")

lsa repo (u "imgs/美子ちゃん.pxv")

toInstance repo mb4 (u "sns/美子ちゃん.pxv.uitlnk")

lsa repo (u "imgs/美子ちゃん.pxv")

//
// importのテスト
//
let importFile = Import.importFile

let srcfiNew = FileInfo("/Users/arinokazuma/work/forest_4edge.png")
importFile repo srcfiNew (u "imgs/forest.png")
lsa repo (u "imgs/forest.png")
lsmb repo (u "imgs/forest.png")

let srcfiDup = FileInfo("/Users/arinokazuma/work/testdata_org/imgs/美子ちゃん.pxv")
importFile repo srcfiDup (u "imgs/newmiko.pxv")
lsa repo (u "imgs/美子ちゃん.pxv")

DInfo.findFInfo repo (u "imgs/美子ちゃん.pxv")
DInfo.findFInfo repo (u "imgs/newmiko.pxv")


let importDir = Import.importDir

importDir repo (DirectoryInfo "/Users/arinokazuma/work/testdata_org") (UDir.fromUit "testdir")

listDupMB repo |> List.iter dispMb

lsa repo (u "scratch.fsx")
listMB repo



let d = UDir.fromUit

lsa repo (u "test1.txt")
lsmb repo (u "test1.txt")

copyDir repo (d "folder2") (d "folder1/copy_dest")

// Blob.fromHashMB repo hash

listFiles (UDir.toDI repo (d "folder2"))

(*
想定するコマンドラインの使い方を考える。

cd repodir
uit init
uit lsdup
uit uniqit mp3/
uit uniqit -all
uit inst study/language/
uit lsh
uit info 2b0b
uit inst sns/img1.png

uit ls sns/img1.png

// -lで他のリンク先の情報も見る
uit ls -l sns/img1.png

// すでにあったらlinkとしてインポート。
uit import /somewhere/file.mp3 music/file.mp3

// .uit/dirs や .uit/hash を更新しつつ移動
uit mv sutudy/language/roman study/roman

// linkとしてコピーして実体はコピーしない
uit cp mp3/roman/somefile.mp3 study/roman/somefile.mp3

// -iで新しく追加した方をinstanceにする
uit cp -i mp3/roman/somefile.mp3 study/roman/somefile.mp3

// ディレクトリ下のメタ情報を更新。存在しない物は追加、変更されているものは…conflictとして扱うか？ちょっとここは未定。
uit update study/download/

*)
