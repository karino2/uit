#load "Common.fs"
#load "Blob.fs"
#load "FInfo.fs"
#load "Action.fs"
#load "InteractiveUtils.fs"
#load "TestUtils.fs"

open Common
open Blob
open FInfo
open Action
open InteractiveUtils
open System.IO
open TestUtils

let u = UPath.fromUit
let lsfi = DInfo.findFInfo

let d = UDir.fromUit

// fsharplint:disable Hints

let repo = DirectoryInfo "./testdata_work" |> repoAt

shellExecute "setuptest.sh" ""
initRecursive repo

// 最初の状態の検査
lsfi repo (u "test1.txt")
|> shouldSome

let dupmbs = listDupMB repo
List.length dupmbs |> should 1

// 再帰的にもinitが出来ている事もこれで確認
instLen dupmbs.Head |> should 3


// 
// uniqit, toInstanceのテスト
// 
uniqIt repo dupmbs.Head


listDupMB repo
let unimb = lsmb repo (u "test1.txt")
instLen unimb |> should 1
linkLen unimb |> should 2

unimb.InstancePathList.Head.Path |> should (u "folder1/test1.txt")
unimb.LinkPathList.Head.Path |> should (u "test1.txt.uitlnk")

DInfo.ls repo (d "")
|> List.map (fun ent->ent.Entry.Type)
|> List.head |> should Link

toInstance repo unimb (u "test1.txt.uitlnk")

let unimb2 = lsmb repo (u "test1.txt")
unimb2.InstancePathList.Head.Path |> should (u "test1.txt")
unimb2.LinkPathList.Head.Path |> should (u "folder1/test1.txt.uitlnk")
instLen unimb2 |> should 1
linkLen unimb2 |> should 2

//
// removeのテスト
//
let remove = Remove.remove

// 最後のInstanceを削除、Linkがちゃんと実体になるか。

let fi1 = UPath.toFileInfo repo (u "folder1/test1.txt.uitlnk")
fi1.Exists |> should true

unimb2.Hash
|> shouldNot (computeFileHash fi1)

remove repo unimb2 (u "test1.txt")

let unimb3 = lsmb repo (u "folder1/test1.txt")

instLen unimb3 |> should 1
linkLen unimb3 |> should 1

let fi2 = UPath.toFileInfo repo (u "folder1/test1.txt")

unimb2.Hash
|> should (computeFileHash fi2)

lsfi repo (u "test1.txt")
|> shouldNone

lsfi repo (u "folder1/test1.txt")
|> shouldSome

lsfi repo (u "folder2/folder3/another_test1.txt.uitlnk")
|> shouldSome

// Linkを削除

remove repo unimb3 (u "folder2/folder3/another_test1.txt.uitlnk")

let unimb4 = lsmb repo (u "folder1/test1.txt")

instLen unimb4 |> should 1
linkLen unimb4 |> should 0

lsfi repo (u "folder1/test1.txt")
|> shouldSome

lsfi repo (u "folder2/folder3/another_test1.txt.uitlnk")
|> shouldNone


//
// Importのテスト
//



//
// CopyDirのテスト
// 
shellExecute "setuptest.sh" ""
initRecursive repo

uniqItAll repo

copyDir repo (d "folder2") (d "folder1/copy_dest")

let cpfs = DInfo.ls repo (d "folder1/copy_dest")
cpfs.Length |> should 2

cpfs
|> List.forall (fun fi->fi.Entry.Type = Link)
|> should true

let cpfs2 = DInfo.ls repo (d "folder1/copy_dest/folder3")
cpfs2.Length |> should 1
cpfs2.Head.Entry.Type |> should Link

let cbmb = lsmb repo (u "folder1/copy_dest/test2.txt")

instLen cbmb |> should 1
linkLen cbmb |> should 1


//
// moveDirのテスト
//
(*
% ls -R testdata/init
folder1		folder2		test1.txt

testdata/init/folder1:
test1.txt	test4.txt

testdata/init/folder2:
folder3		test2.txt	test3.txt

testdata/init/folder2/folder3:
another_test1.txt
*)

shellExecute "setuptest.sh" ""
initRecursive repo

uniqItAll repo

moveDir repo (d "folder2") (d "folder1/move_dest")


DInfo.ls repo (d "folder2") |> should []
let mvfis = DInfo.ls repo (d "folder1/move_dest")

// test2.txt, test3.txtが実体であるか？
mvfis.Length |> should 2

let shouldInstanceExist targetName finfos =
    finfos |> List.find (fun finfo -> finfo.FName = targetName)
    |> (fun finfo-> finfo.Entry.Type) |> should Instance

shouldInstanceExist "test2.txt" mvfis
shouldInstanceExist "test3.txt" mvfis

let mvfis2 = DInfo.ls repo (d "folder1/move_dest/folder3")
mvfis2.Length |> should 1
mvfis2.Head.Entry.Type |> should Link

//
// Ingestのテスト
//

shellExecute "setuptest.sh" ""

init repo

let repoChild = DirectoryInfo "./testdata_work/folder2" |> repoAt
let childdir = (d "folder2")

initRecursive repoChild

listMB repo |> List.length |> should 1

Ingest.ingest repo childdir

// test1.txt
// folder2/test2.txt
// folder2/folder3/another_test1.txt
listMB repo |> List.length |> should 3
listDupMB repo |> List.length |> should 1


//
// listHashWithのテスト
//

// test1のハッシュ
listHashWith repo "3057" |> List.length |> should 1


//
// キャッシュのテスト
//

let setupcache () =
    shellExecute "mkdir" "./testdata_work/.uit/cache" |> printfn "%s"
    shellExecute "mkdir" "./testdata_work/.uit/cache/.uit" |> printfn "%s"
    shellExecute "mv" "./testdata_work/.uit/hash ./testdata_work/.uit/cache/.uit/" |> printfn "%s"
    shellExecute "mv" "./testdata_work/.uit/dirs ./testdata_work/.uit/cache/.uit/" |> printfn "%s"
    File.WriteAllText("./testdata_work/.uit/cachepath.txt", (DirectoryInfo "./testdata_work/.uit/cache").FullName)

shellExecute "setuptest.sh" ""

initRecursive repo

let unimb_cache = lsmb repo (u "test1.txt")
uniqIt repo unimb_cache

// test1.txt.uitlnkになっている
DInfo.findFInfo repo (u "test1.txt")
|> (fun f->f.Value.Entry.Type ) |> should Link

setupcache ()
shellExecute "rm" "./testdata_work/test1.txt.uitlnk"
// test1.txtのタイムスタンプを元に戻すべくコピー元からコピー
shellExecute "cp" "-p ./testdata/init/test1.txt ./testdata_work/test1.txt"

let repowc = DirectoryInfo "./testdata_work" |> repoAtWC
let dircache = DirCache.fromRepo repowc (d "")
let fi = FileInfo "testdata_work/test1.txt"

DirCache.find dircache fi
|> shouldSome
