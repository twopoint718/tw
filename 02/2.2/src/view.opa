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
        <div class=hero-unit>
          <div class=jumbotron>
            <h1><i class="fa fa-bicycle"></i> Biking</h1>
          </div>
        </div>
        <div class=row>
          <div class=col-md-8>{render_table()}</div>    <!-- <4>
          -->
          <div class=col-md-3>{render_input_form()}</div>
        </div>
      </div>

    page_template("Biking", content)                  // <5>
  }
  // end::viewPageLayout[]

  // tag::viewMainTable[]
  function render_table() {
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
        {render_table_rows()}
      </tbody>
    </table>
  }

  function render_table_rows() {
    all_rides = /biking/rides                         // <2>
    it = DbSet.iterator(all_rides)                    // <3>
    Iter.fold(render_table_row, it, <></>)            // <4>
  }

  function render_table_row(row, acc) {               // <5>
    pr = Date.generate_printer("%a., %b. %E, %Y");
                                                      // <6>
    <>
      {acc}
      <tr>
        <td>{row.id}</td>
        <td>{row.user_name}</td>
        <td>{row.distance}</td>
        <td>{Date.to_formatted_string(pr, row.date)}</td>
      </tr>
    </>
  }
  // end::viewMainTable[]

  // tag::viewInputForm[]
  function render_input_form() {
    <form>
      <div class=form-group>
        <label for=name>Name</label>
        <input class=form-control id=name
          placeholder="Enter name">
      </div>
      <div class=form-group>
        <label for=distance>Distance (mi)</label>
        <input type=number class=form-control
          min=0 step=0.1 id=distance
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
