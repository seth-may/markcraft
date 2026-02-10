// Event-sourced shopping cart with discriminated unions
open System

type ProductId = ProductId of Guid
type CartId = CartId of Guid
type Quantity = Quantity of int

type CartItem = {
    ProductId: ProductId
    Name: string
    Price: decimal
    Quantity: Quantity
}

type CartEvent =
    | ItemAdded of CartItem
    | ItemRemoved of ProductId
    | QuantityChanged of ProductId * Quantity
    | CartCleared
    | CheckedOut of DateTime

type CartState = {
    Id: CartId
    Items: Map<ProductId, CartItem>
    IsCheckedOut: bool
    Total: decimal
}

module Cart =
    let empty cartId = {
        Id = cartId
        Items = Map.empty
        IsCheckedOut = false
        Total = 0m
    }

    let apply state event =
        match event with
        | ItemAdded item ->
            let items = state.Items |> Map.add item.ProductId item
            let total = items |> Map.fold (fun acc _ i ->
                let (Quantity q) = i.Quantity
                acc + i.Price * decimal q) 0m
            { state with Items = items; Total = total }
        | ItemRemoved pid ->
            let items = state.Items |> Map.remove pid
            { state with Items = items }
        | QuantityChanged (pid, qty) ->
            match state.Items |> Map.tryFind pid with
            | Some item ->
                let updated = { item with Quantity = qty }
                { state with Items = state.Items |> Map.add pid updated }
            | None -> state
        | CartCleared -> { state with Items = Map.empty; Total = 0m }
        | CheckedOut dt -> { state with IsCheckedOut = true }

    let replay cartId events =
        events |> List.fold apply (empty cartId)

// Pipeline operators
let (|>>) x f = async { let! v = x in return f v }
let (>>=) x f = async { let! v = x in return! f v }

// Async workflow
let processOrder cartId events = async {
    let state = Cart.replay cartId events
    if state.IsCheckedOut then
        return Error "Already checked out"
    elif state.Items.IsEmpty then
        return Error "Cart is empty"
    else
        let event = CheckedOut DateTime.UtcNow
        let final = Cart.apply state event
        printfn "Order total: %M EUR (%d items)"
            final.Total (final.Items.Count)
        return Ok final
}

// Pattern matching with active patterns
let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd
let describe = function
    | Even -> "even" | Odd -> "odd"

[<EntryPoint>]
let main _ =
    let cartId = CartId (Guid.NewGuid())
    let pid = ProductId (Guid.NewGuid())
    let events = [
        ItemAdded { ProductId = pid; Name = "MarkCraft Pro"
                    Price = 9.99m; Quantity = Quantity 2 }
    ]
    let state = Cart.replay cartId events
    printfn "Cart total: %M EUR" state.Total
    0
