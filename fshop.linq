<Query Kind="FSharpProgram" />

type Address = { Street: string; City: string; Postcode: string; }

type StandardPrice = decimal
type DiscountedPrice = decimal * StandardPrice

type Price =  StandardPrice | DiscountedPrice
    
type Product = { SKU: string; Price: decimal; }

type OrderLine = { Product: Product; Quantity: int; LineTotal: decimal; }

type ActiveOrderData = { Lines: OrderLine list; Total: decimal; }
type ShippableOrderData = { Lines: OrderLine list; Total: decimal; ShippingAddress: Address; }
type BillableOrderData = { Lines: OrderLine list; Total: decimal; ShippingAddress: Address; BillingAddress: Address; }

type Order = 
    | EmptyOrder
    | ActiveOrder of ActiveOrderData
    | ShippableOrder of ShippableOrderData
    | BillableOrder of BillableOrderData

let dmpOrderLine orderLine =
    printfn "%ix %s %.2f %.2f" orderLine.Quantity orderLine.Product.SKU orderLine.Product.Price orderLine.LineTotal

let dmpOrder order =
    match order with
    | EmptyOrder ->
        printfn "Empty"
    | ActiveOrder o -> 
        printfn "%i Lines" (o.Lines.Count()) 
        printfn "------------------"
        o.Lines |> List.sortBy (fun l -> l.Product.SKU) |> List.iter dmpOrderLine
        printfn "------------------"
        printfn "Total %.2f" o.Total 
    | _ -> failwith "Not Implemented"
    ""
   
let dmp x = x |> Dump |> ignore

let dmpr x =
    dmp (dmpOrder x)
    x

module List = 
    let singleOrNone predicate list = 
        let results = list |> List.filter predicate
        match results with [] -> None | l -> Some(List.exactlyOne l)

let updateOrderLines line lines = 
    let sku = line.Product.SKU
    let existingLine = lines |> List.singleOrNone (fun l -> l.Product.SKU = sku)
    match existingLine with
    | Some l ->
        let mergedLine = { Product = l.Product; Quantity = l.Quantity + line.Quantity; LineTotal = l.LineTotal + line.LineTotal }
        let otherLines = lines |> List.filter (fun l -> l.Product.SKU <> sku)
        if mergedLine.Quantity < 1 then otherLines else mergedLine :: otherLines
    | None -> 
        line :: lines
            
let updateOrder qty product order = 
    let line = { Product = product; Quantity = qty; LineTotal = product.Price * (decimal qty); }
    match order with
    | EmptyOrder ->
        ActiveOrder { Lines = [line]; Total = line.LineTotal }
    | ActiveOrder o -> 
        let updatedLines = o.Lines |> updateOrderLines line
        ActiveOrder { Lines = updatedLines; Total = updatedLines |> List.sumBy (fun l -> l.LineTotal) }
    | _ -> failwith "Not Implemented"

let productA = { SKU = "A"; Price = 5.00M }
let productB = { SKU = "B"; Price = 10.00M }
let productC = { SKU = "C"; Price = 20.00M }

let order = EmptyOrder

order 
|> dmpr
|> updateOrder 1 productA
|> dmpr
|> updateOrder 1 productB
|> dmpr
|> updateOrder 1 productA
|> dmpr
|> updateOrder 1 productC
|> dmpr
|> updateOrder -1 productB
|> dmpr
|> updateOrder -1 productA
|> dmpr
|> ignore