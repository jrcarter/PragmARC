import GPS


def do_reload():
    if GPS.Project.root().name().lower() == "pragmarc":
        GPS.Project.load("tests/pragmarc-tests.gpr")


def on_project_loaded(proj):
    do_reload()

GPS.Hook("gps_started").add(on_project_loaded)
# GPS.Hook("project_view_changed").add(on_project_loaded)
