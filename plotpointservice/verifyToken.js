import jwt from 'jsonwebtoken'
import config from './config'

export default async function verifyToken (req, res, next) {
  try {
    const jwtHeader = req.headers[config.jwt.header]
    const token = jwtHeader.split(' ')[1]
    const decoded = await
      jwt.verify(token, config.jwt.secret)
    req.user = {
      id: decoded.id,
      username: decoded.username
    }
    next()
  }
  catch (e) {
    res.status(403).end()
  }
}