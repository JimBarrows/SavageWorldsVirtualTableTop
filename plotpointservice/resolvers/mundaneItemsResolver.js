import {mundane_items_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {

  return mundane_items_for_plot_point(obj.id)
}