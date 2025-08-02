import React from 'react'
import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import { BrowserRouter, useNavigate } from 'react-router-dom'
import '@testing-library/jest-dom'
import SignupPage from './SignupPage'

// Mock the auth service
jest.mock('../services/authService', () => ({
  default: {
    register: jest.fn()
  }
}))

// Mock React Router's useNavigate
const mockNavigate = jest.fn()
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}))

import authService from '../services/authService'

describe('SignupPage', () => {
  beforeEach(() => {
    jest.clearAllMocks()
  })

  const renderSignupPage = () => {
    return render(
      <BrowserRouter>
        <SignupPage />
      </BrowserRouter>
    )
  }

  it('renders signup page with title and form', () => {
    renderSignupPage()
    
    expect(screen.getByRole('heading', { name: /sign up/i })).toBeInTheDocument()
    expect(screen.getByText(/create your account/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/username/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/email/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/^password$/i)).toBeInTheDocument()
    expect(screen.getByLabelText(/confirm password/i)).toBeInTheDocument()
  })

  it('renders link to login page', () => {
    renderSignupPage()
    
    const loginLink = screen.getByText(/already have an account/i)
    expect(loginLink).toBeInTheDocument()
    expect(loginLink.closest('a')).toHaveAttribute('href', '/login')
  })

  it('successfully creates account and redirects to login', async () => {
    authService.register.mockResolvedValueOnce({ success: true })
    renderSignupPage()
    
    const usernameInput = screen.getByLabelText(/username/i)
    const emailInput = screen.getByLabelText(/email/i)
    const passwordInput = screen.getByLabelText(/^password$/i)
    const confirmPasswordInput = screen.getByLabelText(/confirm password/i)
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(usernameInput, { target: { value: 'newuser' } })
    fireEvent.change(emailInput, { target: { value: 'new@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(authService.register).toHaveBeenCalledWith({
        username: 'newuser',
        email: 'new@example.com',
        password: 'ValidPassword123!'
      })
    })

    expect(screen.getByText(/account created successfully/i)).toBeInTheDocument()
    
    await waitFor(() => {
      expect(mockNavigate).toHaveBeenCalledWith('/login')
    }, { timeout: 3000 })
  })

  it('displays error message when signup fails', async () => {
    authService.register.mockRejectedValueOnce(new Error('Username already exists'))
    renderSignupPage()
    
    const usernameInput = screen.getByLabelText(/username/i)
    const emailInput = screen.getByLabelText(/email/i)
    const passwordInput = screen.getByLabelText(/^password$/i)
    const confirmPasswordInput = screen.getByLabelText(/confirm password/i)
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(usernameInput, { target: { value: 'existinguser' } })
    fireEvent.change(emailInput, { target: { value: 'existing@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByRole('alert')).toHaveTextContent(/username already exists/i)
    })

    expect(mockNavigate).not.toHaveBeenCalled()
  })

  it('handles network errors gracefully', async () => {
    authService.register.mockRejectedValueOnce(new Error('Network error'))
    renderSignupPage()
    
    const usernameInput = screen.getByLabelText(/username/i)
    const emailInput = screen.getByLabelText(/email/i)
    const passwordInput = screen.getByLabelText(/^password$/i)
    const confirmPasswordInput = screen.getByLabelText(/confirm password/i)
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    fireEvent.change(usernameInput, { target: { value: 'testuser' } })
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.click(submitButton)

    await waitFor(() => {
      expect(screen.getByRole('alert')).toHaveTextContent(/network error/i)
    })
  })
})