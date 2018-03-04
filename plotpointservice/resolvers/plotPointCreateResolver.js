import {create_plot_point} from '../data'

export default function (obj, args, context, graphql) {
  return create_plot_point(args.plotPoint.brief_description, args.plotPoint.description, args.plotPoint.name)
  .then(result => result.id)
}