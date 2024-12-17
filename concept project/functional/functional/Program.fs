open System
open System.IO
open System.Text.Json

// Define types
type Transaction = { Date: string; Category: string; Amount: float }
type Income = { Source: string; Amount: float }
type Budget = { Category: string; Limit: float; Spent: float }
type SavingsGoal = { Goal: string; TargetAmount: float; SavedAmount: float }
type WeeklySpending = { Week: int; Total: float }
type Report = { Budgets: Budget list; SavingsGoals: SavingsGoal list; WeeklySpending: WeeklySpending list }
type SpendingSummary = { TotalSpent: float; CategorySpending: Map<string, float> }

// Helper function to validate input
let validatePositiveFloat (value: float) fieldName =
    if value <= 0.0 then raise (ArgumentException(sprintf "%s must be greater than zero." fieldName))

let validateNonEmptyString (value: string) fieldName =
    if String.IsNullOrWhiteSpace(value) then raise (ArgumentException(sprintf "%s cannot be empty or whitespace." fieldName))

// Functions for modifying state
let addTransaction (transactions: Transaction list) (budgets: Budget list) (date: string) (category: string) (amount: float) =
    try
        validateNonEmptyString date "Date"
        validateNonEmptyString category "Category"
        validatePositiveFloat amount "Amount"

        let parsedDate = DateTime.Parse(date)
        let currentBudget = List.tryFind (fun b -> b.Category = category) budgets

        match currentBudget with
        | Some budget when (transactions |> List.filter (fun t -> t.Category = category) |> List.sumBy (fun t -> t.Amount)) + amount > budget.Limit -> 
            printfn "Error: Transaction exceeds the budget for category '%s'. Transaction cannot be added." category
            transactions, budgets
        | Some budget -> 
            let transaction = { Date = parsedDate.ToString("yyyy-MM-dd"); Category = category; Amount = amount }
            let updatedTransactions = transaction :: transactions
            let updatedBudgets = budgets |> List.map (fun b -> 
                if b.Category = category then { b with Spent = b.Spent + amount } else b
            )
            printfn "Transaction added: %A" transaction
            updatedTransactions, updatedBudgets
        | None -> 
            printfn "Error: No budget set for category '%s'. Please set a budget first." category
            transactions, budgets
    with ex -> 
        printfn "Error adding transaction: %s" ex.Message
        transactions, budgets

let addIncome (incomeSources: Income list) (source: string) (amount: float) =
    try
        validateNonEmptyString source "Source"
        validatePositiveFloat amount "Amount"

        let income = { Source = source; Amount = amount }
        let updatedIncomeSources = income :: incomeSources
        printfn "Income added: %A" income
        updatedIncomeSources
    with ex -> 
        printfn "Error adding income: %s" ex.Message
        incomeSources

let setBudget (budgets: Budget list) (category: string) (limit: float) =
    try
        validateNonEmptyString category "Category"
        validatePositiveFloat limit "Limit"

        let spent = budgets |> List.tryFind (fun b -> b.Category = category) |> Option.map (fun b -> b.Spent) |> Option.defaultValue 0.0
        let budget = { Category = category; Limit = limit; Spent = spent }
        let updatedBudgets = budget :: (budgets |> List.filter (fun b -> b.Category <> category))
        printfn "Budget set for category '%s': Limit = %.2f" category limit
        updatedBudgets
    with ex -> 
        printfn "Error setting budget: %s" ex.Message
        budgets

let setSavingsGoal (savingsGoals: SavingsGoal list) (goal: string) (targetAmount: float) =
    try
        validateNonEmptyString goal "Goal"
        validatePositiveFloat targetAmount "Target Amount"

        let savingsGoal = { Goal = goal; TargetAmount = targetAmount; SavedAmount = 0.0 }
        let updatedSavingsGoals = savingsGoal :: (savingsGoals |> List.filter (fun g -> g.Goal <> goal))
        printfn "Savings goal set for '%s': Target = %.2f" goal targetAmount
        updatedSavingsGoals
    with ex -> 
        printfn "Error setting savings goal: %s" ex.Message
        savingsGoals

let calculateTotalSpent (transactions: Transaction list) (category: string) =
    transactions |> List.filter (fun t -> t.Category = category) |> List.sumBy (fun t -> t.Amount)

let trackBudgetUtilization (budgets: Budget list) =
    budgets |> List.iter (fun budget -> 
        let percentageSpent = (budget.Spent / budget.Limit) * 100.0
        if percentageSpent >= 90.0 then
            printfn "Alert: You have spent %.2f%% of your budget for category '%s'. You are nearing your limit!" percentageSpent budget.Category
        elif percentageSpent >= 80.0 then
            printfn "Warning: You have spent %.2f%% of your budget for category '%s'. You are over 80%% of the budget." percentageSpent budget.Category)

let calculateWeeklySpending (transactions: Transaction list) =
    try
        if List.isEmpty transactions then
            printfn "No transactions to calculate weekly spending."
            []
        else
            let referenceDate = DateTime.Parse(transactions.Head.Date)
            let daysBetween (date1: DateTime) (date2: DateTime) = (date2 - date1).Days

            // Calculate weekly spending using a functional approach
            let weeklyTotals = 
                transactions
                |> List.fold (fun acc transaction ->
                    let transactionDate = DateTime.Parse(transaction.Date)
                    let weekNumber = abs(daysBetween referenceDate transactionDate) / 7

                    match List.tryFind (fun (week, _) -> week = weekNumber) acc with
                    | Some (week, total) ->
                        List.map (fun (w, t) -> if w = week then (w, t + transaction.Amount) else (w, t)) acc
                    | None ->
                        (weekNumber, transaction.Amount) :: acc
                ) []

            // Convert the list of tuples to WeeklySpending records
            List.map (fun (week, total) -> { Week = week; Total = total }) weeklyTotals
    with ex -> 
        printfn "Error calculating weekly spending: %s" ex.Message
        []

let generateWeeklyReport (transactions: Transaction list) =
    try
        let weeklySpending = calculateWeeklySpending transactions
        if List.isEmpty weeklySpending then 
            printfn "No weekly spending data available."
        else
            printfn "\nWeekly Spending Report:"
            List.iter (fun week -> printfn "Week %d: %.2f" week.Week week.Total) weeklySpending
    with ex -> 
        printfn "Error generating weekly report: %s" ex.Message

let recommendWeeklySavings (savingsGoals: SavingsGoal list) (incomeSources: Income list) =
    let totalIncome = incomeSources |> List.sumBy (fun i -> i.Amount)
    let totalTargetAmount = savingsGoals |> List.sumBy (fun g -> g.TargetAmount)
    let totalSavedAmount = savingsGoals |> List.sumBy (fun g -> g.SavedAmount)
    let remainingToSave = totalTargetAmount - totalSavedAmount

    if remainingToSave <= 0.0 then printfn "You have already met your savings goals."
    else
        let weeklyIncome = totalIncome / 4.0
        let weeklySavingsRecommendation = remainingToSave / 4.0
        printfn "Total weekly income: %.2f" weeklyIncome
        printfn "Recommended weekly savings for your goals: %.2f" weeklySavingsRecommendation


let generateSpendingSummary (transactions: Transaction list) =
    let totalSpent = transactions |> List.sumBy (fun t -> t.Amount)
    let categorySpending = 
        transactions
        |> List.groupBy (fun t -> t.Category)
        |> List.map (fun (category, trans) -> (category, trans |> List.sumBy (fun t -> t.Amount)))
        |> Map.ofList
    printfn "\nSpending Summary:"
    printfn "Total Spent: %.2f" totalSpent
    categorySpending |> Map.iter (fun category amount -> printfn "Category '%s': %.2f" category amount)

let viewDailyTransactions (transactions: Transaction list) (date: string) =
    try
        validateNonEmptyString date "Date"
        let parsedDate = DateTime.Parse(date)
        let dailyTransactions = transactions |> List.filter (fun t -> DateTime.Parse(t.Date) = parsedDate)
        if dailyTransactions.IsEmpty then
            printfn "No transactions found for %s." date
        else
            printfn "\nTransactions on %s:" date
            dailyTransactions |> List.iter (fun transaction -> printfn "Category: %s, Amount: %.2f" transaction.Category transaction.Amount)
    with ex -> 
        printfn "Error viewing daily transactions: %s" ex.Message
let exportReport (filePath: string) (budgets: Budget list) (savingsGoals: SavingsGoal list) (weeklySpending: WeeklySpending list) =
    try
        validateNonEmptyString filePath "File Path"
        let report = { Budgets = budgets; SavingsGoals = savingsGoals; WeeklySpending = weeklySpending }
        let options = JsonSerializerOptions(WriteIndented = true)
        let json = JsonSerializer.Serialize(report, options)
        File.WriteAllText(filePath, json)
        printfn "Report exported to %s" filePath
    with ex -> 
        printfn "Error exporting report: %s" ex.Message

let importTransactions (filePath: string) =
    try
        validateNonEmptyString filePath "File Path"
        if not (File.Exists filePath) then raise (FileNotFoundException(sprintf "File not found: %s" filePath))
        let json = File.ReadAllText(filePath)
        let importedTransactions = JsonSerializer.Deserialize<Transaction list>(json)
        importedTransactions
    with ex -> 
        printfn "Error importing transactions: %s" ex.Message
        []

let interactiveMode () =
    let rec loop (transactions, incomeSources, budgets, savingsGoals) =
        printfn "\nOptions:"
        printfn "1. Add transaction"
        printfn "2. Add income"
        printfn "3. Set budget"
        printfn "4. Set savings goal"
        printfn "5. Import transactions from file"
        printfn "6. Export report to file"
        printfn "7. Generate weekly report"
        printfn "8. Generate spending summary"
        printfn "9. Recommend weekly savings"
        printfn "10. View daily transactions"
        printfn "11. Exit"
        match Console.ReadLine() with
        | "1" -> 
            // Add transaction
            printf "Enter date (yyyy-MM-dd): "
            let date = Console.ReadLine()
            printf "Enter category: "
            let category = Console.ReadLine()
            printf "Enter amount: "
            let amount = float (Console.ReadLine())
            let updatedTransactions, updatedBudgets = addTransaction transactions budgets date category amount
            loop (updatedTransactions, incomeSources, updatedBudgets, savingsGoals)
        | "2" -> 
            printf "Enter source of income: "
            let source = Console.ReadLine()
            printf "Enter amount of income: "
            let amount = float (Console.ReadLine())
            loop (transactions, addIncome incomeSources source amount, budgets, savingsGoals)
        | "3" -> 
            printf "Enter category: "
            let category = Console.ReadLine()
            printf "Enter limit: "
            let limit = float (Console.ReadLine())
            loop (transactions, incomeSources, setBudget budgets category limit, savingsGoals)
        | "4" -> 
            printf "Enter savings goal: "
            let goal = Console.ReadLine()
            printf "Enter target amount: "
            let targetAmount = float (Console.ReadLine())
            loop (transactions, incomeSources, budgets, setSavingsGoal savingsGoals goal targetAmount)
        | "5" -> 
            printf "Enter file path to import from: "
            let filePath = Console.ReadLine()
            loop (importTransactions filePath, incomeSources, budgets, savingsGoals)
        | "6" -> 
            printf "Enter file path to export to: "
            let filePath = Console.ReadLine()
            exportReport filePath budgets savingsGoals (calculateWeeklySpending transactions)
            loop (transactions, incomeSources, budgets, savingsGoals)
        | "7" -> 
            generateWeeklyReport transactions
            loop (transactions, incomeSources, budgets, savingsGoals)
        | "8" -> 
            generateSpendingSummary transactions
            loop (transactions, incomeSources, budgets, savingsGoals)
      
        | "9" -> 
            recommendWeeklySavings savingsGoals incomeSources
            loop (transactions, incomeSources, budgets, savingsGoals)
        | "10" -> 
            printf "Enter date (yyyy-MM-dd): "
            let date = Console.ReadLine()
            viewDailyTransactions transactions date
            loop (transactions, incomeSources, budgets, savingsGoals)
        | "11" -> 
            printfn "Goodbye!"
        | _ -> 
            printfn "Invalid option. Please choose again."
            loop (transactions, incomeSources, budgets, savingsGoals)

    loop ([], [], [], [])

// Start the interactive mode
interactiveMode ()