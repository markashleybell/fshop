<Query Kind="FSharpProgram" />

type Price =  StandardPrice | DiscountedPrice
type Address = { Street: string; City: string; Postcode: string; }
type ShippingMethod = { Code: string; Price: decimal; }
type PaymentMethod = Card | Cash | Cheque

type StandardPrice = decimal
type DiscountedPrice = decimal * StandardPrice

type Product = { SKU: string; Price: decimal; }

type OrderLine = { Product: Product; Quantity: int; LineTotal: decimal; }
type ShippingData = { ShippingAddress: Address; ShippingMethod: ShippingMethod; }

type ActiveOrderData = { Lines: OrderLine list; Total: decimal; }
type ValidOrderData = { Lines: OrderLine list; ShippingData: ShippingData; BillingAddress: Address; Total: decimal; }
type CompletedOrderData = { Lines: OrderLine list; ShippingData: ShippingData; BillingAddress: Address; Total: decimal; }

type Order = 
    | EmptyOrder
    | ActiveOrder of ActiveOrderData
    | ValidOrder of ValidOrderData
    | CompletedOrder of CompletedOrderData

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
    | ValidOrder o -> 
        printfn "%i Lines" (o.Lines.Count()) 
        printfn "------------------"
        o.Lines |> List.sortBy (fun l -> l.Product.SKU) |> List.iter dmpOrderLine
        printfn "------------------"
        printfn "Total %.2f" o.Total 
        printfn "------------------"
        printfn "Ship To %s" o.ShippingData.ShippingAddress.Postcode 
        printfn "Bill To %s" o.BillingAddress.Postcode 
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
        let mergedLine = { Product = l.Product; Quantity = l.Quantity + line.Quantity; LineTotal = l.LineTotal + line.LineTotal; }
        let otherLines = lines |> List.filter (fun l -> l.Product.SKU <> sku)
        if mergedLine.Quantity < 1 then otherLines else mergedLine :: otherLines
    | None -> 
        line :: lines
        
let calculateTotal orderLines =
    orderLines |> List.sumBy (fun l -> l.LineTotal)
  
let updateItems qty product order = 
    let line = { Product = product; Quantity = qty; LineTotal = product.Price * (decimal qty); }
    match order with
    | EmptyOrder ->
        ActiveOrder { Lines = [line]; Total = line.LineTotal; }
    | ActiveOrder o ->
        let updatedLines = o.Lines |> updateOrderLines line
        if updatedLines.Length = 0 then EmptyOrder else ActiveOrder { Lines = updatedLines; Total = updatedLines |> calculateTotal; }
    | ValidOrder o -> 
        let updatedLines = o.Lines |> updateOrderLines line
        if updatedLines.Length = 0 then EmptyOrder else ValidOrder { o with Lines = updatedLines; Total = updatedLines |> calculateTotal; }
    | CompletedOrder o -> 
        CompletedOrder o
        
let updateShippingDetails address shippingMethod order =
    let shippingData = { ShippingAddress = address; ShippingMethod = shippingMethod; }
    match order with
    | EmptyOrder ->
        order
    | ActiveOrder o ->
        ValidOrder { Lines = o.Lines; ShippingData = shippingData; BillingAddress = address; Total = o.Total; }
    | ValidOrder o -> 
        ValidOrder { o with ShippingData = shippingData; BillingAddress = address; }
    | CompletedOrder o -> 
        CompletedOrder o
    
        
let productA = { SKU = "A"; Price = 5.00M }
let productB = { SKU = "B"; Price = 10.00M }
let productC = { SKU = "C"; Price = 20.00M }

let address = { Street = "123 Some Street"; City = "Sometown"; Postcode = "SM1 100"; }
let shippingMethod = { Code = "RM24"; Price = 2.50M; }

let order = EmptyOrder

order |> dmpr
|> updateItems 1 productA |> dmpr
//|> updateItems 1 productB |> dmpr
|> updateItems 1 productA |> dmpr
//|> updateItems 1 productC |> dmpr
//|> updateItems -1 productB |> dmpr 
|> updateItems -1 productA |> dmpr
|> updateShippingDetails address shippingMethod |> dmpr
|> updateItems -1 productA |> dmpr
|> ignore

