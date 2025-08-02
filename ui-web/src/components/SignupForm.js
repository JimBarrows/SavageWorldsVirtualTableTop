import React, { Component } from 'react'
import PropTypes from 'prop-types'
import { TextFormGroup, EmailFormGroup, PasswordFormGroup } from 'bootstrap-react-components'
import { validateSignupForm } from '../utils/validation'

class SignupForm extends Component {
  static propTypes = {
    onSubmit: PropTypes.func.isRequired
  }

  state = {
    formData: {
      username: '',
      email: '',
      password: '',
      confirmPassword: ''
    },
    errors: {},
    isSubmitting: false,
    submitError: null
  }

  handleChange = (field) => (event) => {
    const value = event.target.value
    this.setState(prevState => ({
      formData: {
        ...prevState.formData,
        [field]: value
      },
      errors: {
        ...prevState.errors,
        [field]: undefined
      },
      submitError: null
    }))
  }

  handleSubmit = async (event) => {
    event.preventDefault()
    
    const errors = validateSignupForm(this.state.formData)
    
    if (Object.keys(errors).length > 0) {
      this.setState({ errors })
      return
    }

    this.setState({ isSubmitting: true, submitError: null })

    try {
      await this.props.onSubmit({
        username: this.state.formData.username,
        email: this.state.formData.email,
        password: this.state.formData.password
      })
      
      // Reset form on success
      this.setState({
        formData: {
          username: '',
          email: '',
          password: '',
          confirmPassword: ''
        },
        isSubmitting: false
      })
    } catch (error) {
      this.setState({ 
        submitError: error.message,
        isSubmitting: false 
      })
    }
  }

  render() {
    const { formData, errors, isSubmitting, submitError } = this.state

    return (
      <form onSubmit={this.handleSubmit} id="signup-form">
        {submitError && (
          <div className="alert alert-danger" role="alert">
            {submitError}
          </div>
        )}

        <div className="mb-3">
          <TextFormGroup
            id="username"
            label="Username"
            name="username"
            value={formData.username}
            onChange={this.handleChange('username')}
            required={true}
            disabled={isSubmitting}
          />
          {errors.username && (
            <div className="invalid-feedback d-block">
              {errors.username}
            </div>
          )}
        </div>

        <div className="mb-3">
          <EmailFormGroup
            id="email"
            label="Email"
            name="email"
            value={formData.email}
            onChange={this.handleChange('email')}
            required={true}
            disabled={isSubmitting}
          />
          {errors.email && (
            <div className="invalid-feedback d-block">
              {errors.email}
            </div>
          )}
        </div>

        <div className="mb-3">
          <PasswordFormGroup
            id="password"
            label="Password"
            name="password"
            value={formData.password}
            onChange={this.handleChange('password')}
            required={true}
            disabled={isSubmitting}
          />
          {errors.password && (
            <div className="invalid-feedback d-block">
              {errors.password}
            </div>
          )}
        </div>

        <div className="mb-3">
          <PasswordFormGroup
            id="confirmPassword"
            label="Confirm Password"
            name="confirmPassword"
            value={formData.confirmPassword}
            onChange={this.handleChange('confirmPassword')}
            required={true}
            disabled={isSubmitting}
          />
          {errors.confirmPassword && (
            <div className="invalid-feedback d-block">
              {errors.confirmPassword}
            </div>
          )}
        </div>

        <button
          type="submit"
          className="btn btn-primary w-100"
          disabled={isSubmitting}
        >
          {isSubmitting ? 'Signing up...' : 'Sign Up'}
        </button>
      </form>
    )
  }
}

export default SignupForm