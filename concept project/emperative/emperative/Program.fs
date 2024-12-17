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

// Mutable lists
let mutable transactions: Transaction list = []
let mutable incomeSources: Income list = []
let mutable budgets: Budget list = []
let mutable savingsGoals: SavingsGoal list = []

// Helper function to validate input
let validatePositiveFloat (value: float) fieldName =
    if value <= 0.0 then raise (ArgumentException(sprintf "%s must be greater than zero." fieldName))

let validateNonEmptyString (value: string) fieldName =
    if String.IsNullOrWhiteSpace(value) then raise (ArgumentException(sprintf "%s cannot be empty or whitespace." fieldName))

// Function to calculate total spent for a category
let calculateTotalSpent (category: string) =
    transactions
    |> List.filter (fun t -> t.Category = category)
    |> List.sumBy (fun t -> t.Amount)

// Function to track budget utilization and provide alerts
let trackBudgetUtilization () =
    for budget in budgets do
        let percentageSpent = (budget.Spent / budget.Limit) * 100.0
        if percentageSpent >= 90.0 then
            printfn "Alert: You have spent %.2f%% of your budget for category '%s'. You are nearing your limit!" percentageSpent budget.Category
        elif percentageSpent >= 80.0 then
            printfn "Warning: You have spent %.2f%% of your budget for category '%s'. You are over 80%% of the budget." percentageSpent budget.Category

// Function to add a transaction
let addTransaction (date: string) (category: string) (amount: float) =
    try
        validateNonEmptyString date "Date"
        validateNonEmptyString category "Category"
        validatePositiveFloat amount "Amount"

        let parsedDate = DateTime.Parse(date)
        let currentBudget = budgets |> List.tryFind (fun b -> b.Category = category)

        match currentBudget with
        | Some budget when calculateTotalSpent category + amount > budget.Limit ->
            printfn "Error: Transaction exceeds the budget for category '%s'. Transaction cannot be added." category
        | Some budget ->
            let transaction = { Date = parsedDate.ToString("yyyy-MM-dd"); Category = category; Amount = amount }
            transactions <- transaction :: transactions
            budgets <- budgets |> List.map (fun b -> if b.Category = category then { b with Spent = b.Spent + amount } else b)
            printfn "Transaction added: %A" transaction
            trackBudgetUtilization ()
        | None -> printfn "Error: No budget set for category '%s'. Please set a budget first." category
    with ex -> printfn "Error adding transaction: %s" ex.Message

// Function to add income
let addIncome (source: string) (amount: float) =
    try
        validateNonEmptyString source "Source"
        validatePositiveFloat amount "Amount"

        let income = { Source = source; Amount = amount }
        incomeSources <- income :: incomeSources
        printfn "Income added: %A" income
    with ex -> printfn "Error adding income: %s" ex.Message

// Function to calculate total income
let calculateTotalIncome () =
    incomeSources |> List.sumBy (fun i -> i.Amount)

// Function to calculate weekly savings recommendation
let recommendWeeklySavings () =
    try
        let totalIncome = calculateTotalIncome ()
        let totalTargetAmount = savingsGoals |> List.sumBy (fun g -> g.TargetAmount)
        let totalSavedAmount = savingsGoals |> List.sumBy (fun g -> g.SavedAmount)

        let remainingToSave = totalTargetAmount - totalSavedAmount
        if remainingToSave <= 0.0 then printfn "You have already met your savings goals."
        else
            let weeklyIncome = totalIncome / 4.0
            let weeklySavingsRecommendation = remainingToSave / 4.0

            printfn "Total weekly income: %.2f" weeklyIncome
            printfn "Recommended weekly savings for your goals: %.2f" weeklySavingsRecommendation
    with ex -> printfn "Error calculating weekly savings: %s" ex.Message

// Function to set a budget
let setBudget (category: string) (limit: float) =
    try
        validateNonEmptyString category "Category"
        validatePositiveFloat limit "Limit"

        let spent = calculateTotalSpent category
        let budget = { Category = category; Limit = limit; Spent = spent }
        budgets <- budget :: (budgets |> List.filter (fun b -> b.Category <> category))
        printfn "Budget set for category '%s': Limit = %.2f" category limit
        trackBudgetUtilization ()
    with ex -> printfn "Error setting budget: %s" ex.Message

// Function to set a savings goal
let setSavingsGoal (goal: string) (targetAmount: float) =
    try
        validateNonEmptyString goal "Goal"
        validatePositiveFloat targetAmount "Target Amount"

        let savingsGoal = { Goal = goal; TargetAmount = targetAmount; SavedAmount = 0.0 }
        savingsGoals <- savingsGoal :: (savingsGoals |> List.filter (fun g -> g.Goal <> goal))
        printfn "Savings goal set for '%s': Target = %.2f" goal targetAmount
    with ex -> printfn "Error setting savings goal: %s" ex.Message

// Function to calculate weekly spending s
let calculateWeeklySpending () =
    try
        if transactions.IsEmpty then 
            printfn "No transactions to calculate weekly spending."
            []
        else
            let referenceDate = DateTime.Parse(transactions.Head.Date)
            let daysBetween (date1: DateTime) (date2: DateTime) = (date2 - date1).Days

            // Use a mutable list to track weekly totals
            let mutable weeklyTotals = []

            for transaction in transactions do
                let transactionDate = DateTime.Parse(transaction.Date)
                let weekNumber = abs(daysBetween referenceDate transactionDate) / 7

                // Check if the week already exists in the list
                match List.tryFind (fun (week, _) -> week = weekNumber) weeklyTotals with
                | Some (week, total) ->
                    // Update the total for the week
                    weeklyTotals <- weeklyTotals |> List.map (fun (w, t) -> if w = week then (w, t + transaction.Amount) else (w, t))
                | None ->
                    // Add a new entry for the week
                    weeklyTotals <- (weekNumber, transaction.Amount) :: weeklyTotals

            // Convert the list to a list of WeeklySpending records
            [ for (week, total) in weeklyTotals -> { Week = week; Total = total } ]
    with ex -> 
        printfn "Error calculating weekly spending: %s" ex.Message
        []

// Function to generate weekly report
let generateWeeklyReport () =
    try
        let weeklySpending = calculateWeeklySpending ()
        if weeklySpending.IsEmpty then 
            printfn "No weekly spending data available."
        else
            printfn "\nWeekly Spending Report:"
            for week in weeklySpending do
                printfn "Week %d: %.2f" week.Week week.Total
    with ex -> 
        printfn "Error generating weekly report: %s" ex.Message



// Function to generate summaries of spending
let generateSpendingSummary () =
    try
        let totalSpent = transactions |> List.sumBy (fun t -> t.Amount)
        let categorySpending = transactions
                                |> List.groupBy (fun t -> t.Category)
                                |> List.map (fun (category, trans) -> (category, trans |> List.sumBy (fun t -> t.Amount)))
                                |> Map.ofList

        printfn "\nSpending Summary:"
        printfn "Total Spent: %.2f" totalSpent
        for category in categorySpending do
            printfn "Category '%s': %.2f" category.Key category.Value
    with ex -> printfn "Error generating spending summary: %s" ex.Message

// Function to display daily transactions
let viewDailyTransactions (date: string) =
    try
        validateNonEmptyString date "Date"
        let parsedDate = DateTime.Parse(date)
        let dailyTransactions = transactions |> List.filter (fun t -> DateTime.Parse(t.Date) = parsedDate)

        if dailyTransactions.IsEmpty then
            printfn "No transactions found for %s." date
        else
            printfn "\nTransactions on %s:" date
            for transaction in dailyTransactions do
                printfn "Category: %s, Amount: %.2f" transaction.Category transaction.Amount
    with ex -> printfn "Error viewing daily transactions: %s" ex.Message

// Function to generate and export a report
let exportReport (filePath: string) =
    try
        validateNonEmptyString filePath "File Path"
        let weeklySpending = calculateWeeklySpending ()
        let report = { Budgets = budgets; SavingsGoals = savingsGoals; WeeklySpending = weeklySpending }
        let options = JsonSerializerOptions(WriteIndented = true)
        let json = JsonSerializer.Serialize(report, options)
        File.WriteAllText(filePath, json)
        printfn "Report exported to %s" filePath
    with ex -> printfn "Error exporting report: %s" ex.Message

// Function to import transactions from a JSON file
let importTransactions (filePath: string) =
    try
        validateNonEmptyString filePath "File Path"
        if not (File.Exists filePath) then raise (FileNotFoundException(sprintf "File not found: %s" filePath))

        let json = File.ReadAllText(filePath)
        let importedTransactions = JsonSerializer.Deserialize<Transaction list>(json)
        transactions <- importedTransactions @ transactions
        printfn "Transactions imported from %s" filePath
    with ex -> printfn "Error importing transactions: %s" ex.Message

// Interactive loop for user inputs
let rec interactiveMode () =
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
    let choice = Console.ReadLine()
    match choice with
    | "1" -> 
        try
            printf "Enter date (yyyy-MM-dd): "
            let date = Console.ReadLine()
            printf "Enter category: "
            let category = Console.ReadLine()
            printf "Enter amount: "
            let amount = float (Console.ReadLine())
            addTransaction date category amount
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "2" -> 
        try
            printf "Enter source of income: "
            let source = Console.ReadLine()
            printf "Enter amount of income: "
            let amount = float (Console.ReadLine())
            addIncome source amount
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "3" -> 
        try
            printf "Enter category: "
            let category = Console.ReadLine()
            printf "Enter limit: "
            let limit = float (Console.ReadLine())
            setBudget category limit
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "4" -> 
        try
            printf "Enter savings goal: "
            let goal = Console.ReadLine()
            printf "Enter target amount: "
            let targetAmount = float (Console.ReadLine())
            setSavingsGoal goal targetAmount
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "5" -> 
        try
            printf "Enter file path to import from: "
            let filePath = Console.ReadLine()
            importTransactions filePath
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "6" -> 
        try
            printf "Enter file path to export to: "
            let filePath = Console.ReadLine()
            exportReport filePath
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "7" -> 
        generateWeeklyReport ()
        interactiveMode ()
    | "8" -> 
        generateSpendingSummary ()
        interactiveMode ()
    
    | "9" -> 
        recommendWeeklySavings ()
        interactiveMode ()
    | "10" -> 
        try
            printf "Enter date (yyyy-MM-dd): "
            let date = Console.ReadLine()
            viewDailyTransactions date
        with ex -> printfn "Error: %s" ex.Message
        interactiveMode ()
    | "11" -> printfn "Goodbye!"
    | _ -> 
        printfn "Invalid option. Please choose again."
        interactiveMode ()

// Start the interactive mode
interactiveMode ()