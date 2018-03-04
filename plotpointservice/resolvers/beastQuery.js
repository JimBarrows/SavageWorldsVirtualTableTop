import {beasts} from '../data'

export default function (obj, args, context, graphql) {

  return beasts(obj.id)
}