module Hashtable

open System.Drawing
open System.Drawing.Imaging
open System.IO

let primes = [| 3; 7; 11; 17; 23; 29; 37; 47; 59; 71; 89; 107; 131; 163; 197; 239; 293; 353; 431; 521; 631; 761; 919; 1103; 1327; 1597; 1931; 2333; 2801; 3371; 4049; 4861; 5839; 7013; 8419; 10103; 12143; 14591; 17519; 21023; 25229; 30293; 36353; 43627; 52361; 62851; 75431; 90523; 108631; 130363; 156437; 187751; 225307; 270371; 324449; 389357; 467237; 560689; 672827; 807403; 968897; 1162687; 1395263; 1674319; 2009191; 2411033; 2893249; 3471899; 4166287; 4999559; 5999471; 7199369 |]

type Hashtable =
    { BucketPointers : int option []
      BucketEntriesCounts : int []
      Entries : (bool * int * int) [] }

let create (bucketsCount : int option) (pairs : (int * int) []) : Hashtable =

    let size =
        match bucketsCount with
        | Some size -> size
        | None ->
            let mutable i = 0
            while primes.[i] < pairs.Length do i <- i + 1
            primes.[i]
    
    let buckets =
        let accu = Array.init size (fun i -> ResizeArray())
        for (k, v) in pairs do
            accu.[k % size].Add((k, v))
        Array.map (fun (bucket : ResizeArray<int * int>) -> bucket.ToArray()) accu

    let entries = ResizeArray()
    let pointers = ResizeArray()
    for bucket in buckets do
        if bucket.Length = 0 then
            pointers.Add(None)
        else
            pointers.Add(Some entries.Count)
            for i = 0 to bucket.Length - 1 do
                let key, value = bucket.[i]
                let hasNext = i <> bucket.Length - 1
                entries.Add(hasNext, key, value)

    { BucketPointers = pointers.ToArray()
      BucketEntriesCounts = Array.map (fun (bucket : (int * int) []) -> bucket.Length) buckets
      Entries = entries.ToArray() }

let outputHashtableStat m n (table : Hashtable) =

    let occupied = table.Entries.Length
    let maxEntriesCountInSingleBucket = Array.max table.BucketEntriesCounts
    
    printfn "full matrix elements : %d, occupied matrix elements: %d, density : %f, buckets count : %d, max entries count in single bucket : %d"
        (m * n)
        table.Entries.Length
        (float occupied / (float n * float m))
        table.BucketPointers.Length
        maxEntriesCountInSingleBucket

let scaleImage (n : int) (img : Bitmap) =
    let newImg = new Bitmap(n * img.Width, n * img.Height)
    use g = Graphics.FromImage(newImg)
    for x = 0 to img.Width - 1 do
        for y = 0 to img.Height - 1 do
            use brush = new SolidBrush(img.GetPixel(x, y))
            g.FillRectangle(brush, n * x, n * y, n, n)
    newImg

let outputInt16Array (os : TextWriter) (name : string) (ary : int array) =
    fprintf os "let %s = [| " name
    for i = 0 to ary.Length - 1 do  
        if i <> 0 then
            fprintf os "; "
        fprintf os "%ds" ary.[i]
    fprintfn os " |]"

let outputHashtable (f : TextWriter) (name : string) (table : Hashtable) =
    outputInt16Array f (name + "_buckets") (Array.map (function Some n -> n | None -> -1) table.BucketPointers)
    let entries =
        let accu = ResizeArray()
        for (hasNext, key, value) in table.Entries do
            accu.Add(if hasNext then ~~~ key else key)
            accu.Add(value)
        accu.ToArray()
    outputInt16Array f (name + "_entries") entries

let outputHashtableImage (path : string) (table : Hashtable) =
    let buckets = table.BucketEntriesCounts
    let lineLength = 256
    let lineHeight = 8
    let lineCount = buckets.Length / lineLength + (if buckets.Length % lineLength > 0 then 1 else 0)
    use bmp = new Bitmap(lineLength, lineHeight * lineCount)
    for i = 0 to lineCount - 1 do
        for j = 0 to lineLength - 1 do
            let x = j
            let bucketIndex = lineLength * i + j
            for k = 0 to lineHeight - 1 do
                let y = lineHeight * (i + 1) - 1 - k
                let color =
                    if bucketIndex < buckets.Length then
                        if k < buckets.[bucketIndex] then
                            match k with
                            | 0 -> Color.Green
                            | 1 -> Color.Green
                            | 2 -> Color.Yellow
                            | _ -> Color.Red
                        else Color.Black
                    else Color.FromArgb(0xFF303030)
                bmp.SetPixel(x, y, color)
    use scaled = scaleImage 4 bmp
    try scaled.Save(path, ImageFormat.Png) // this fails when the image is too big
    with _ -> Printf.eprintfn "failed to save hashtable image %s" path
