module Hashtable

open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D

let primes = [| 3; 7; 11; 17; 23; 29; 37; 47; 59; 71; 89; 107; 131; 163; 197; 239; 293; 353; 431; 521; 631; 761; 919; 1103; 1327; 1597; 1931; 2333; 2801; 3371; 4049; 4861; 5839; 7013; 8419; 10103; 12143; 14591; 17519; 21023; 25229; 30293; 36353; 43627; 52361; 62851; 75431; 90523; 108631; 130363; 156437; 187751; 225307; 270371; 324449; 389357; 467237; 560689; 672827; 807403; 968897; 1162687; 1395263; 1674319; 2009191; 2411033; 2893249; 3471899; 4166287; 4999559; 5999471; 7199369 |]

type Hashtable =
    { BucketPointers : int option []
      BucketCounts : int []
      Entries : (bool * int * int) [] }

let create (size : int option) (matrix : int option [] []) : Hashtable =

    let occupied =
        let mutable accu = 0
        for row in matrix do
            for elem in row do
                if elem.IsSome then accu <- accu + 1
        accu

    let size =
        match size with
        | Some size -> size
        | None ->
            let mutable i = 0
            while primes.[i] < occupied do i <- i + 1
            primes.[i]

    let buckets = Array.init size (fun i -> ResizeArray())
    let m = matrix.Length
    let n = matrix.[0].Length

    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            match matrix.[i].[j] with
            | Some v ->
                let k = n * i + j
                let h = k % size
                buckets.[h].Add((k, v))
            | None -> ()
    
    let buckets = Array.map (fun (bucket : ResizeArray<int * int>) -> bucket.ToArray()) buckets

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
      BucketCounts = Array.map (fun (bucket : (int * int) []) -> bucket.Length) buckets
      Entries = entries.ToArray() }

let outputHashtableStat (matrix : int option [] []) (table : Hashtable) =

    let m = matrix.Length
    let n = if matrix.Length > 0 then matrix.[0].Length else 0
    let occupied = table.Entries.Length
    let maxEntriesCountInSingleBucket = Array.max table.BucketCounts
    
    printfn "full-matrix elements : %d, occupied matrix elements: %d, density : %f, buckets count : %d, max entries count in single bucket : %d"
        (n * m)
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

let outputHashtableImage (path : string) (table : Hashtable) =
    let maxBucketLength = Array.max table.BucketCounts

    let bmp = new Bitmap(table.BucketPointers.Length, maxBucketLength)
    for i = 0 to table.BucketPointers.Length - 1 do
        for j = 0 to maxBucketLength - table.BucketCounts.[i] - 1 do
            bmp.SetPixel(i, j, Color.Black)
        for j = maxBucketLength - table.BucketCounts.[i] to maxBucketLength - 1 do
            let color =
                match maxBucketLength - 1 - j with
                | 0 -> Color.Green
                | 1 -> Color.Green
                | 2 -> Color.Yellow
                | _ -> Color.Red
            bmp.SetPixel(i, j, color)
    let scaled = scaleImage 4 bmp
    scaled.Save(path, ImageFormat.Png)
