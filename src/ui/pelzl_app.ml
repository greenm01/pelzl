let app mode = {
  Mosaic.init = Pelzl_model.init mode;
  update = Pelzl_update.update;
  view = Pelzl_view.view;
  subscriptions = Pelzl_subscriptions.subscriptions;
}
