import {hand_weapons_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {

  return hand_weapons_for_plot_point(obj.id)
}