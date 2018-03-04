import {power_by_id} from '../data'

export default function (obj, args, context, graphql) {
  return power_by_id(obj.power_id)
}