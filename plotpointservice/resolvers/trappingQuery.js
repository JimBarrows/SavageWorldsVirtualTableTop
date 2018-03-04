import {trapping_by_id} from '../data'

export default function (obj, args, context, graphql) {
  return trapping_by_id(obj.trapping_id)
}