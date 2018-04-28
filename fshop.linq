<Query Kind="FSharpProgram" />

type Price =  StandardPrice | DiscountedPrice
type Address = { Street: string; City: string; Postcode: string; }
type ShippingMethod = { Price: decimal; }
type PaymentMethod = Card | Cash | Cheque

type StandardPrice = decimal
type DiscountedPrice = decimal * StandardPrice

type Product = { SKU: string; Price: decimal; }

type OrderLine = { Product: Product; Quantity: int; LineTotal: decimal; }
type ShippingData = { ShippingAddress: Address; ShippingMethod: ShippingMethod; }

type EmptyOrderData = { ShippingData: ShippingData option; BillingAddress: Address option; }
type ActiveOrderData = { Lines: OrderLine list; Total: decimal; }
type ShippableOrderData = { Lines: OrderLine list; ShippingData: ShippingData; Total: decimal; }
type BillableOrderData = { Lines: OrderLine list; ShippingData: ShippingData; BillingAddress: Address; Total: decimal; }
type CompletedOrderData = { Lines: OrderLine list; ShippingData: ShippingData; BillingAddress: Address; Total: decimal; }

type Order = 
    | EmptyOrder of EmptyOrderData
    | ActiveOrder of ActiveOrderData
    | ShippableOrder of ShippableOrderData
    | BillableOrder of BillableOrderData
    | CompletedOrder of CompletedOrderData

let dmpOrderLine orderLine =
    printfn "%ix %s %.2f %.2f" orderLine.Quantity orderLine.Product.SKU orderLine.Product.Price orderLine.LineTotal

let dmpOrder order =
    match order with
    | EmptyOrder o ->
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
        let mergedLine = { Product = l.Product; Quantity = l.Quantity + line.Quantity; LineTotal = l.LineTotal + line.LineTotal; }
        let otherLines = lines |> List.filter (fun l -> l.Product.SKU <> sku)
        if mergedLine.Quantity < 1 then otherLines else mergedLine :: otherLines
    | None -> 
        line :: lines
        
let calculateTotal orderLines =
    orderLines |> List.sumBy (fun l -> l.LineTotal)
  
let updateOrder qty product order = 
    let line = { Product = product; Quantity = qty; LineTotal = product.Price * (decimal qty); }
    match order with
    | EmptyOrder o ->
        match o.ShippingData with
        | None -> ActiveOrder { Lines = [line]; Total = line.LineTotal; }
        | Some sd -> match o.BillingAddress with
                     | None -> ShippableOrder { Lines = [line]; Total = line.LineTotal; ShippingData = sd; }
                     | Some ba -> BillableOrder { Lines = [line]; Total = line.LineTotal; ShippingData = sd; BillingAddress = ba }
    | ActiveOrder o ->
        let updatedLines = o.Lines |> updateOrderLines line
        if updatedLines.Length = 0 then EmptyOrder { ShippingData = None; BillingAddress = None; } else ActiveOrder { Lines = updatedLines; Total = updatedLines |> calculateTotal; }
    | ShippableOrder o -> 
        let updatedLines = o.Lines |> updateOrderLines line
        if updatedLines.Length = 0 then EmptyOrder { ShippingData = Some(o.ShippingData); BillingAddress = None; } else ShippableOrder { o with Lines = updatedLines; Total = updatedLines |> calculateTotal; }
    | BillableOrder o -> 
        let updatedLines = o.Lines |> updateOrderLines line
        if updatedLines.Length = 0 then EmptyOrder { ShippingData = Some(o.ShippingData); BillingAddress = Some(o.BillingAddress); } else BillableOrder { o with Lines = updatedLines; Total = updatedLines |> calculateTotal; }
    | CompletedOrder o -> 
        CompletedOrder o
        
let productA = { SKU = "A"; Price = 5.00M }
let productB = { SKU = "B"; Price = 10.00M }
let productC = { SKU = "C"; Price = 20.00M }

let order = EmptyOrder { ShippingData = None; BillingAddress = None; }

order |> dmpr
|> updateOrder 1 productA |> dmpr
//|> updateOrder 1 productB |> dmpr
|> updateOrder 1 productA |> dmpr
//|> updateOrder 1 productC |> dmpr
//|> updateOrder -1 productB |> dmpr 
|> updateOrder -1 productA |> dmpr
|> ignore

