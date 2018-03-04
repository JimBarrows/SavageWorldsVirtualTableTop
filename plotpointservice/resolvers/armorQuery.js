import {armor_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {
  return armor_for_plot_point(obj.id)
}