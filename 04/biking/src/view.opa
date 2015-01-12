import stdlib.themes.bootstrap
import stdlib.themes.bootstrap.font-awesome

module View {
  // tag::viewPageLayout[]
  function page_template(title, content) {
    html =
      <div>                                           <!-- <1>
        -->
        <div class="navbar navbar-fixed-top">
          <div class=navbar-inner>
            <div class=container>
              <a class=brand href="./index.html"></a>
            </div>
          </div>
        </div>                                        <!-- <2>
        -->
        <div id=#main class=container-fluid>
          {content}
        </div>
      </div>
    Resource.page(title, html)                        // <3>
  }

  function default_page() {
    content =
      <div>
        <div class="hero-unit">
          <div class=jumbotron>
            <h1><i class="fa fa-bicycle"></i> Biking</h1>
          </div>
        </div>
        <div class=row>
          <div class=col-md-8>{main_table()}</div>    <!-- <4>
          -->
          <div class=col-md-3>{input_form()}</div>
        </div>
      </div>

    page_template("Biking", content)                  // <5>
  }
  // end::viewPageLayout[]

  // tag::viewMainTable[]
  function main_table() {
    <table class="table table-hover">
      <thead>
        <tr>
          <th>ID</th>
          <th>Name</th>
          <th>Distance ridden</th>
          <th>Date ridden</th>
        </tr>
      </thead>                                      <!-- <1>
      -->
      <tbody>
        {table_rows()}
      </tbody>
    </table>
  }

  function table_rows() {
    dbset(ride, _) all_rides = /biking/rides          // <2>
    it = DbSet.iterator(all_rides)                    // <3>
    Iter.fold(table_row, it, <></>)                   // <4>
  }

  function table_row(r, acc) {                        // <5>
    pr = Date.generate_printer("%a., %b. %E, %Y");
                                                      // <6>
    <>
      {acc}
      <tr>
        <td>{r.id}</td>
        <td>{r.user_name}</td>
        <td>{r.distance}</td>
        <td>{Date.to_formatted_string(pr, r.date)}</td>
      </tr>
    </>
  }
  // end::viewMainTable[]

  // tag::viewInputForm[]
  function input_form() {
    <form>
      <div class=form-group>
        <label for=name>Name</label>
        <input class=form-control id=name
          placeholder="Enter name">
      </div>
      <div class=form-group>
        <label for=distance>Distance (mi)</label>
        <input type=number class=form-control
          min=0 step="0.1" id=distance
          placeholder="Enter distance">
      </div>
      <div class=form-group>
        <label for=date>Date</label>
        <input type=date class=form-control id=date
          placeholder="2015-01-13">
      </div>
      <button type=submit class="btn btn-default"
        onclick={function(_) {                        // <1>
          name = Dom.get_value(#name)
          distance = Dom.get_value(#distance)
          date = Dom.get_value(#date)
          Model.create_ride(name, distance, date)
        }}>
        Submit
      </button>
    </form>
  }
  // end::viewInputForm[]
}
