import {beast_skills} from '../data'

export default function (obj, args, context, graphql) {

  return beast_skills(obj.id)
}