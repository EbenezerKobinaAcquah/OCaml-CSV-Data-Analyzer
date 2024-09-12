(* csv_reader.ml *)

(* Type to represent a CSV row as a list of strings *)
type csv_row = string list

(* Read CSV data from a file and return as a list of rows *)
let read_csv_file filename : csv_row list =
  try
    let file = open_in filename in
    let csv_data = Csv.input_all (Csv.of_channel file) in
    close_in file;
    csv_data
  with
    | Sys_error err -> print_endline ("Error reading file: " ^ err); []

(* Safely convert string to float *)
let safe_float_of_string s =
  try float_of_string s with
  | Failure _ -> 0.0

(* Debug: Print the CSV data *)
let print_csv_data data =
  List.iteri (fun i row ->
    Printf.printf "Row %d: %s\n" i (String.concat ", " row)
  ) data

(* Sum the values in a column of the CSV, e.g., total sales *)
let sum_column (data : csv_row list) column_index =
  List.fold_left (fun acc row ->
    if List.length row > column_index then
      acc +. safe_float_of_string (List.nth row column_index)
    else
      acc
  ) 0.0 data

(* Calculate the average of a column's values *)
let average_column data column_index =
  let total = sum_column data column_index in
  let count = List.length (List.filter (fun row -> List.length row > column_index) data) in
  if count > 0 then total /. float_of_int count
  else 0.0

(* Find the maximum value in a column *)
let max_in_column data column_index =
  List.fold_left (fun acc row ->
    if List.length row > column_index then
      let value = safe_float_of_string (List.nth row column_index) in
      if value > acc then value else acc
    else acc
  ) min_float data

(* Recommend action based on average sales *)
let recommend_sales_action avg_sales =
  if avg_sales > 150.0 then
    "Sales are strong. Consider increasing inventory."
  else if avg_sales < 100.0 then
    "Sales are low. Consider promotional discounts."
  else
    "Sales are steady. Maintain current strategy."

(* Main program to read CSV, analyze data, and provide decisions *)
let () =
  let filename = "sales_data.csv" in
  let csv_data = read_csv_file filename in

  (* Debug: Print CSV data *)
  print_csv_data csv_data;

  (* Ensure there is data to analyze *)
  if List.length csv_data > 1 then
    (* Skip the header row *)
    let data = List.tl csv_data in

    (* Analytics: Calculate total sales, average price, max price *)
    let total_sales = sum_column data 1 in
    let avg_price = average_column data 2 in
    let max_price = max_in_column data 2 in

    (* Print the analytics results *)
    Printf.printf "Total Sales: %.2f\n" total_sales;
    Printf.printf "Average Price: %.2f\n" avg_price;
    Printf.printf "Maximum Price: %.2f\n" max_price;

    (* Provide recommendation based on average sales *)
    let avg_sales = total_sales /. float_of_int (List.length data) in
    let recommendation = recommend_sales_action avg_sales in
    Printf.printf "Recommendation: %s\n" recommendation
  else
    print_endline "No data available for analysis."
