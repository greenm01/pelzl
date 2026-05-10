let app ?editor_runner mode = {
  Mosaic.init = Pelzl_model.init mode;
  update = Pelzl_update.update ?editor_runner;
  view = Pelzl_view.view;
  subscriptions = Pelzl_subscriptions.subscriptions;
}
