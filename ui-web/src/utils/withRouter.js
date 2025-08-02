import React from 'react'
import { useNavigate, useLocation, useParams } from 'react-router-dom'

// Higher order component to provide router props to class components
// This is needed because React Router v6 only provides hooks
export function withRouter(Component) {
  function ComponentWithRouterProp(props) {
    const navigate = useNavigate()
    const location = useLocation()
    const params = useParams()
    
    return (
      <Component
        {...props}
        router={{ location, navigate, params }}
        navigate={navigate}
        location={location}
        params={params}
      />
    )
  }

  return ComponentWithRouterProp
}