
#load "Common.fs"
open Common
open System.IO
// fsharplint:disable Hints



let repo = { Path = DirectoryInfo "/Users/arinokazuma/work/testdata" }
let mikochan = UPath "sns/美子ちゃん.pxv"

//
// Init
//

init repo

//
// upath2binfo
//

upath2binfo repo mikochan

upath2binfo repo mikochan
|> Option.map bi2allpes
|> Option.map (List.map pe2path)


upath2binfo repo mikochan
|> Option.map bi2instances
|> Option.map (List.map pe2path)


//
// listHash
//

let mb2upaths mb =
    let ip =
        mb.InstancePathList
        |> List.map (fun pe-> pe.Path)
    let rp =
        mb.LinkPathList
        |> List.map (fun pe-> pe.Path)
    List.append ip rp

listHash repo
listMF repo
|> List.map mb2upaths |> List.concat

listMF repo
listDupMF repo


listHashWith repo "2b0b"
listMF repo


//
//  ToLinkOne, trial
//

// init repo
// let mikochan = UPath "sns/美子ちゃん.pxv"

let dups = listDupMF repo
uniqIt repo dups.Head
listDupMF repo


let dispMb (mb:ManagedBlob) =
    printfn "Hash: %s" (hash2string mb.Hash)
    printf "Inst: "
    mb.InstancePathList |> List.iter (fun pe->printf "%A " pe.Path)
    printf "\nLinks: "
    mb.LinkPathList |> List.iter (fun pe->printf "%A " pe.Path)
    printfn ""

let lsa repo upath =
    let opbinfo = toFInfo repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (toBInfo repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> dispMb mb
    | _ -> ()

let lsmb repo upath =
    let opbinfo = toFInfo repo upath
                |> Option.map (fun fi->fi.Hash)
                |> Option.map (toBInfo repo)
    match opbinfo with
    | (Some (ManagedBlob mb)) -> mb
    | _ -> failwith("not managed path")


let mb = lsmb repo (UPath "imgs/美子ちゃん.pxv")

toInstance repo mb (UPath "imgs/美子ちゃん.pxv.uitlnk")

lsa repo (UPath "imgs/美子ちゃん.pxv")

toInstance repo mb (UPath "sns/美子ちゃん.pxv.uitlnk")

lsa repo (UPath "imgs/美子ちゃん.pxv")

mb


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

*)
