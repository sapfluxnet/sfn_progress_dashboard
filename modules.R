# sites value box module
## ui function
sitesvbOutput <- function(id, label = "Sites_valuebox") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # valuebox output
  valueBoxOutput(ns("site_number"))
}

## server function
sitesvb <- function(input, output, session, data) {
  site_number_value <- length(unique(data[['si_code']]))
  output$site_number <- renderValueBox({
    valueBox(
      value = site_number_value,
      subtitle = 'Sites',
      icon = icon('dot-circle-o', lib = 'font-awesome'),
      color = 'aqua',
      width = 4
    )
  })
}

# countries value box module
## ui function
countriesvbOutput <- function(id, label = "Country_valuebox") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # valuebox output
  valueBoxOutput(ns("country_number"))
}

## server function
countriesvb <- function(input, output, session, data) {
  country_number_value <- length(unique(data[['si_country']]))
  output$country_number <- renderValueBox({
    valueBox(
      value = country_number_value,
      subtitle = 'Countries',
      icon = icon('globe', lib = 'font-awesome'),
      color = 'light-blue',
      width = 4
    )
  })
}

# contributors value box module
## ui function
contributorsvbOutput <- function(id, label = "Contributors_valuebox") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  # valuebox output
  valueBoxOutput(ns("contributor_number"))
}

## server function
contributorsvb <- function(input, output, session, data) {
  contributor_number_value <- length(
    unique(
      c(
        unique(
          paste(site_md[['si_contact_firstname']],
                site_md[['si_contact_lastname']], sep = ' ')
        ),
        unique(
          paste(site_md[['si_addcontr_firstname']],
                site_md[['si_addcontr_lastname']], sep = ' ')
        ))
    )
  )
  output$contributor_number <- renderValueBox({
    valueBox(
      value = contributor_number_value,
      subtitle = 'Contributors',
      icon = icon('users', lib = 'font-awesome'),
      color = 'blue',
      width = 4
    )
  })
}
