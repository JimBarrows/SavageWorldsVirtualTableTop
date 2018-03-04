import {trappings_effects_for_trapping} from '../data'

export default function (obj, args, context, graphql) {
  return trappings_effects_for_trapping(obj.trapping_id)
}