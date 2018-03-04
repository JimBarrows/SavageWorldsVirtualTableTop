import {arcane_backgrounds_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {
  return arcane_backgrounds_for_plot_point(obj.id)
}