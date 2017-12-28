// tag::typeHouse[]
type House = "Atreides" | "Corrino" | "Harkonnen";
type Planet = "Caladan" | "Giedi Prime" | "Kaitain"
// end::typeHouse[]

// tag::funcHomeworld[]
function homeworld(house : House) : Planet {
  switch(house) {
    case "Atreides": return "Caladan"
    case "Corrino": return "Kaitain"
    case "Harkonnen": return "Giedi Prime"
  }
}
// end::funcHomeworld[]
