let app ?editor_runner ?on_mode_switch ?initial_model mode = {
  Mosaic.init = Pelzl_model.init ?initial_model mode;
  update = Pelzl_update.update ?editor_runner ?on_mode_switch;
  view = Pelzl_view.view;
  subscriptions = Pelzl_subscriptions.subscriptions;
}
