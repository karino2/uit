

#load "Common.fs"
#load "Blob.fs"
#load "FInfo.fs"
#load "Action.fs"
#load "InteractiveUtils.fs"
#load "CommandLine.fs"
#load "TestUtils.fs"

open Common
open Blob
open FInfo
open Action
open InteractiveUtils
open System.IO
open CommandLine
open TestUtils

let u = UPath.fromUit
let ud = UDir.fromUit
// fsharplint:disable Hints

shellExecute "setuptest.sh" ""
let repo = DirectoryInfo "./testdata_work" |> repoAt



//
// cacheの開発
//


// no .uit, no cache
DirectoryInfo "./testdata_work" |> repoAt |> DirCache.cacheRepo


// .uit with no .uit/cache, no cache.
initRecursive repo
DirectoryInfo "./testdata_work" |> repoAt |> DirCache.cacheRepo


// .uit with .uit/cache, cache

let setupcache () =
    shellExecute "mkdir" "./testdata_work/.uit/cache" |> printfn "%s"
    shellExecute "mkdir" "./testdata_work/.uit/cache/.uit" |> printfn "%s"
    shellExecute "mv" "./testdata_work/.uit/hash ./testdata_work/.uit/cache/.uit/" |> printfn "%s"
    shellExecute "mv" "./testdata_work/.uit/dirs ./testdata_work/.uit/cache/.uit/" |> printfn "%s"


setupcache ()

DirectoryInfo "./testdata_work" |> repoAt |> DirCache.cacheRepo



// create .uit/cache with folder2 cache.

shellExecute "setuptest.sh" ""

initEmpty repo

let folder2FI = DirectoryInfo "./testdata_work/folder2"
repoAt folder2FI |> initRecursive

Ingest.ingest repo (UDir.fromDI repo folder2FI)
setupcache ()


// .uit/cacheがある状態で作り直し。repoは古い状態でinitされてるのでobsoleteの可能性がある…
let repowc = DirectoryInfo "./testdata_work" |> repoAt

DirCache.cacheRepo repowc



DirCache.fromRepo repowc (ud "folder2")
DirCache.fromRepo repowc (ud "folder1")

let dircache = DirCache.fromRepo repowc (ud "folder2")

dircache.Value



let fi = FileInfo "testdata_work/folder2/test2.txt"
fi.LastWriteTime

DirCache.findCacheFromDict dircache.Value "test2.txt" fi.LastWriteTime
DirCache.findCacheFromDict dircache.Value "test2.txt" (fi.LastWriteTime.AddDays 1.0)


fi.Name

DirCache.find dircache fi

DirCache.find None fi

// link pathのケースのテスト

shellExecute "setuptest.sh" ""
initRecursive repo

let unimb = lsmb repo (u "test1.txt")
uniqIt repo unimb


lsmb repo (u "test1.txt")
// test1.txt.uitlnkになっている

DInfo.ls repo (ud "")
|> List.head
|> FInfo.resolveName

setupcache ()
shellExecute "rm" "./testdata_work/test1.txt.uitlnk"
// test1.txtのタイムスタンプを元に戻すべくコピー元からコピー
shellExecute "cp" "-p ./testdata/init/test1.txt ./testdata_work/test1.txt"

let repowc2 = DirectoryInfo "./testdata_work" |> repoAt

let dircache = DirCache.fromRepo repowc2 (ud "")

DInfo.ls (DirCache.cacheRepo repowc2).Value (ud "")
|> List.map (fun finf -> ((FInfo.resolveName finf), finf))
|> dict

let fi = FileInfo "testdata_work/test1.txt"


dircache
fi.LastWriteTime

DirCache.find dircache fi

(FileInfo "testdata_work/folder1/test1.txt").LastWriteTime

// loggerのテスト
shellExecute "setuptest.sh" ""

initRecursiveWithLogger repo (fun str -> printf "%s" str)

//
// childが必要な類のテスト
//

let repoChild = DirectoryInfo "./testdata_work/folder2" |> repoAt

// dotUitExistのテスト


init repo

initRecursive repoChild

Ingest.dotUitExist (DirectoryInfo "./testdata_work/folder2")
Ingest.dotUitExist (DirectoryInfo "./testdata_work/folder1")
Ingest.dotUitExist (DirectoryInfo ".")


 
//
// Incoporateの開発用
//
listMB repo |> List.length


Ingest.ingest repo (ud "folder2")


listMB repo

listDupMB repo |> List.length

UDir.fromOSPath "./folder2"
(ud "folder2")

open System

DateTime.Now.ToString()


// zip周りのテスト
#r "nuget: System.IO.Compression"
#r "nuget: System.IO.Compression.ZipFile"
open System.IO.Compression

initRecursive repo

// 手でhash/29をzipしていろいろ調査。


let binary = string2bytes "2925b1cac7ccce4fdfe13dc51c3963d82fea3cee31db34037a992d9bb4eeb92a"
let binary2 = string2bytes "2925b1cac7ccce4fdfe13dc51c3963d82fea3cee31db34037a992d9bb4eeb92b"

let hash1 = (Hash binary)
let hash2 = (Hash binary2)



let zipfile = hashReadZip repo hash1


hashEntry zipfile hash1
hashEntry zipfile hash2


let (Some ent) = hashEntry zipfile hash1


readlines ent

let fi = DirectoryInfo(hashRootStr(repo)).EnumerateFiles() |> Seq.head
 

// saveTextZip関連


let tempfi1 = FileInfo "./testdata_work/test_tmp1.zip"
saveTextZip tempfi1 "hoge.txt" "fugafuga\nhegahega"
saveTextZip tempfi1 "added.txt" "this is added"
saveTextZip tempfi1 "hoge.txt" "Updated!\nfugafuga\nhegahega"


tempfi1.Length
removeEntry tempfi1 "hoge.txt"

// tempfi作り直さないと反映されない
tempfi1.Length
removeEntry tempfi1 "added.txt"


isEmptyZip tempfi1


let tmpzip = ZipFile.Open(tempfi1.FullName, ZipArchiveMode.Read)
tmpzip.Entries |> Seq.isEmpty

tmpzip.Dispose()

tempfi1.Length

tempfi1.Exists

tempfi1.Name

trimEnd ".zip" tempfi1.Name


listHashWith repo "3057"

listMB repo

lshmb repo "3057"

//
// findTopUitDir
//

let di = DirectoryInfo "./testdata_work"
di.Parent

di.Parent.Parent.Parent.Parent // /Users
di.Parent.Parent.Parent.Parent.Parent // /
di.Parent.Parent.Parent.Parent.Parent.Parent //  nulll

di.Parent.Parent.Parent.Parent.Parent.Parent = null

di.EnumerateDirectories(".uit")

Seq.toList( di.Parent.EnumerateDirectories(".uit") ) = []


init repo
initRecursive (repoAt (DirectoryInfo "./testdata_work/folder2"))


findTopUitDir di.Parent

findCurrentUitDir di

findCurrentUitDir (DirectoryInfo "./testdata_work/folder2/folder3")


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
