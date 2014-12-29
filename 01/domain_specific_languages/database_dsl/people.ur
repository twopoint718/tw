(* vim: ft=sml
*
* Compile with:
* urweb -dbms sqlite -db people.db people
* sqlite3 people.db < seed.sql
*)

(* tag::tablePage[] *)
table people : { Id : int, First : string, Last : string }
  PRIMARY KEY Id

fun list () =
    rows <- queryX (SELECT * FROM people)    (* <1> *)
            (fn row => <xml><tr>
              <td>{[row.People.Id]}</td>
              <td>{[row.People.First]}</td>  (* <2> *)
              <td>{[row.People.Last]}</td>
            </tr></xml>);

    return <xml>
      <table>
        <tr>
          <th>ID</th>
          <th>First</th>
          <th>Last</th>
        </tr>
        {rows}
      </table>
    </xml>
(* end::tablePage[] *)

fun main () =
    xml <- list ();
    return <xml><body>
      <h1>List of people</h1>
      {xml}
    </body></xml>
