open Core

type temperature = float

type cardinal = N | S | E | W

type direction =
  | One of cardinal
  | Two of cardinal * cardinal
  | Three of cardinal * cardinal * cardinal

let direction_of_string s = 
  let cardinal_of_char c =
    match c with
    | 'N' -> N
    | 'S' -> S
    | 'E' -> E
    | 'W' -> W
    | _ -> failwith "Bad cardinal"
  in
  if s = "---"
  then None
  else
    Some (
      match List.map (String.to_list s) ~f:cardinal_of_char with
      | [ a ] ->  One a
      | [ a; b ] ->  Two (a, b)
      | [ a; b; c ] ->  Three (a, b, c)
      | _ -> failwith "Bad direction")

type row =
  { rowID : string
  ; dateTime : Time.t
  ; tempOut : temperature
  ; hiTemp : temperature
  ; lowTemp : temperature
  ; outHum : float
  ; dewPt : float
  ; windSpeed : float
  ; windDir : direction option
  ; windRun : float
  ; hiSpeed : float
  ; hiDir : direction option
  ; windChill : float
  ; heatIndex : float
  ; tHWIndex : float
  ; bar : float
  ; rain : float
  ; rainRate : float
  ; heatDD : float
  ; coolDD : float
  ; inTemp : temperature
  ; inHum : float
  ; inDew : float
  ; inHeat : float
  ; inEMC : float
  ; inAirDensity : float
  ; windSamp : float
  ; windTx : float
  ; iSSRecpt : float
  ; arcInt : float } 

let parse_row s =
  match String.split s ~on:',' with
  [ rowID
  ; dateTime
  ; tempOut
  ; hiTemp
  ; lowTemp
  ; outHum
  ; dewPt
  ; windSpeed
  ; windDir
  ; windRun
  ; hiSpeed
  ; hiDir
  ; windChill
  ; heatIndex
  ; tHWIndex
  ; bar
  ; rain
  ; rainRate
  ; heatDD
  ; coolDD
  ; inTemp
  ; inHum
  ; inDew
  ; inHeat
  ; inEMC
  ; inAirDensity
  ; windSamp
  ; windTx
  ; iSSRecpt
  ; arcInt ] ->
    { rowID
    ; dateTime = Time.parse dateTime ~fmt:"%m/%d/%Y %I:%M:%S %p" ~zone:Time.Zone.utc
    ; tempOut = Float.of_string tempOut
    ; hiTemp = Float.of_string hiTemp
    ; lowTemp = Float.of_string lowTemp
    ; outHum = Float.of_string outHum
    ; dewPt = Float.of_string dewPt
    ; windSpeed = Float.of_string windSpeed
    ; windDir = direction_of_string windDir
    ; windRun = Float.of_string windRun
    ; hiSpeed = Float.of_string hiSpeed
    ; hiDir = direction_of_string hiDir
    ; windChill = Float.of_string windChill
    ; heatIndex = Float.of_string heatIndex
    ; tHWIndex = Float.of_string tHWIndex
    ; bar = Float.of_string bar
    ; rain = Float.of_string rain
    ; rainRate = Float.of_string rainRate
    ; heatDD = Float.of_string heatDD
    ; coolDD = Float.of_string coolDD
    ; inTemp = Float.of_string inTemp
    ; inHum = Float.of_string inHum
    ; inDew = Float.of_string inDew
    ; inHeat = Float.of_string inHeat
    ; inEMC = Float.of_string inEMC
    ; inAirDensity = Float.of_string inAirDensity
    ; windSamp = Float.of_string windSamp
    ; windTx = Float.of_string windTx
    ; iSSRecpt = Float.of_string iSSRecpt
    ; arcInt  = Float.of_string arcInt  }
  | _ -> failwith "Bad row"

let () =
  match In_channel.read_lines "Local_Weather_Data.csv" with
  | [] -> failwith "Empty file"
  | _header :: rows ->
    let rows = List.map rows ~f:parse_row in
    let total_number = List.length rows in
    let warm_days =
      List.count rows ~f:(fun row ->
        row.hiTemp > 60.)
    in
    printf "The fraction of warm days was %f\n"
      (Float.of_int warm_days /. Float.of_int total_number)
