notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning",
  notificationItem(
    text = "5 new users today",
    icon("users")
  ),
  notificationItem(
    text = "12 items delivered",
    icon("truck"),
    status = "success"
  ),
  notificationItem(
    text = "Server load at 86%",
    icon = icon("exclamation-triangle"),
    status = "warning"
  )
)
