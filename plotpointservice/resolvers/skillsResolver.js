import {skills_for_plot_point} from '../data'

export default function (obj, args, context, graphql) {

  return skills_for_plot_point(obj.id)
}