import {powers_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {

  return powers_for_plot_point(obj.id)
}