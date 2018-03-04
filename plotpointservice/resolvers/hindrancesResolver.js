import {hindrances_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {

  return hindrances_for_plot_point(obj.id)
}