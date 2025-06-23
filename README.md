# nhp_evidence_maps

Visit the app: https://connect.strategyunitwm.nhs.uk/nhp_evidence_map/

## About

Evidence maps are a way of visually displaying the body of literature in a specific field in order to show gaps in evidence or study characteristics. 

This shiny application will allow for the interactive exploration of the literature through tools such as evidence maps and text searching. 

The application currently uses a dataset containing information about each evidence, such as author, publication date, and study characteristics.
This information is then used to create an evidence map of all of the evidence as well as a free text search in order to search the evidence in the dataset by their title or author.

## Developer notes

<details><summary>Click for detail.</summary>

### Update pinned data

The underlying data for this app can be updated independently of the app.
The data is stored as a 'pin' on Posit Connect.
The app will read the data from the pin using [{pins}](https://pins.rstudio.com/) when the user reaches the app.
You can overwrite the existing pin and it will create a new version; you can see and revert to earlier versions of the pin if needed.

This is some illustrative code to update the pinned data:

``` r
# Connect to board
board <- pins::board_connect()

# Check existing pin
pin_name <- "matt.dray/nhp_evidence_map_data"
board |> pins::pin_exists(pin_name)  # logical
board |> pins::pin_read(pin_name) |> str(1)  # list structure
board |> pins::pin_versions(pin_name)  # active and past versions

# Read spreadsheet into list

file <- "spreadsheet.xlsx"
sheet_names <- readxl::excel_sheets(file)

sheets_list <- purrr::map(
  sheet_names, 
  \(x) suppressMessages(readxl::read_xlsx(file, sheet = x))
) |> 
  purrr::set_names(sheet_names)

# Write to pin with custom 'notes' metadata
board |> pins::pin_write(
  sheets_list,
  pin_name,
  metadata = list(notes = "A reminder of changes/reason for upload."),
  type = "rds"  # otherwise it may autodetect json
)

# Confirm upload
board |> pins::pin_versions(pin_name)  # should see new version
pins::pin_meta(board, pin_name)[["user"]][["notes"]]  # custom notes metadata
```

### Deploy

Create a `.Renviron` in the project root containing the keys found in `.Renviron.example`.
This will allow you to connect to the board on the server so you can fetch the pinned data.
Ask a member of the Data Science team if you don't know what values you need.
If you're working in RStudio and connected to the correct server, you won't need to do this.

Run the `dev/03_deploy.R` script to deploy to Posit Connect.
You can read more about [deploying to Posit Connect](https://docs.posit.co/connect/how-to/publish-shiny-app/) and [deploying a Golem app](https://engineering-shiny.org/deploy.html).

</details>