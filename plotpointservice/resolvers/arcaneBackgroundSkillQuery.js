import {arcane_background_skill} from '../data'

export default function (obj, args, context, graphql) {
  return arcane_background_skill(obj.skill_id)
}