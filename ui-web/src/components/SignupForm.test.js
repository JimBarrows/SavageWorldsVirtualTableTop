import React from 'react'
import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import '@testing-library/jest-dom'
import SignupForm from './SignupForm'

describe('SignupForm', () => {
  let mockOnSubmit

  beforeEach(() => {
    mockOnSubmit = jest.fn()
  })

  it('renders all required form fields', () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    expect(container.querySelector('#FormControl-email-EmailFormGroup-email')).toBeInTheDocument()
    expect(container.querySelector('#FormControl-password-PasswordFormGroup-password')).toBeInTheDocument()
    expect(container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /sign up/i })).toBeInTheDocument()
  })

  it('displays error when email is empty', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/email is required/i)).toBeInTheDocument()
    })
    expect(mockOnSubmit).not.toHaveBeenCalled()
  })

  it('displays error when email is invalid', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'invalid-email' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/please enter a valid email address/i)).toBeInTheDocument()
    })
    expect(mockOnSubmit).not.toHaveBeenCalled()
  })

  it('displays error when passwords do not match', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'DifferentPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/passwords do not match/i)).toBeInTheDocument()
    })
    expect(mockOnSubmit).not.toHaveBeenCalled()
  })

  it('displays error when password is too weak', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'weak' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'weak' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/password must be at least 8 characters/i)).toBeInTheDocument()
    })
    expect(mockOnSubmit).not.toHaveBeenCalled()
  })

  it('calls onSubmit with form data when all fields are valid', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(mockOnSubmit).toHaveBeenCalledWith({
        email: 'test@example.com',
        password: 'ValidPassword123!'
      })
    })
  })

  it('disables submit button while form is submitting', async () => {
    const slowSubmit = jest.fn(() => new Promise(resolve => setTimeout(resolve, 100)))
    const { container } = render(<SignupForm onSubmit={slowSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    expect(submitButton).toBeDisabled()
    expect(submitButton).toHaveTextContent(/signing up/i)

    await waitFor(() => {
      expect(submitButton).not.toBeDisabled()
      expect(submitButton).toHaveTextContent(/sign up/i)
    })
  })

  it('displays server error message when submission fails', async () => {
    const failingSubmit = jest.fn(() => Promise.reject(new Error('Email already exists')))
    const { container } = render(<SignupForm onSubmit={failingSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(emailInput, { target: { value: 'existing@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/email already exists/i)).toBeInTheDocument()
    })
  })

  it('clears error messages when user starts typing', async () => {
    const { container } = render(<SignupForm onSubmit={mockOnSubmit} />)
    
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    // Submit with empty email to trigger error
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByText(/email is required/i)).toBeInTheDocument()
    })

    // Start typing in email field
    fireEvent.change(emailInput, { target: { value: 't' } })

    // Error should be cleared
    expect(screen.queryByText(/email is required/i)).not.toBeInTheDocument()
  })
})