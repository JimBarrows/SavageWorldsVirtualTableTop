import React from 'react'
import { render, screen, fireEvent, waitFor, act } from '@testing-library/react'
import { BrowserRouter } from 'react-router-dom'
import '@testing-library/jest-dom'

// Import mocked service
import authService from '../services/authService'

// Now import components after mocks are set up
import SignupPage from './SignupPage'

// Mock the auth service before importing components
jest.mock('../services/authService')

// Mock React Router's useNavigate
const mockNavigate = jest.fn()
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}))


describe('SignupPage', () => {
  beforeEach(() => {
    jest.clearAllMocks()
    // Reset all mocks
    mockNavigate.mockClear()
    // Setup default mock implementation
    authService.register = jest.fn()
  })
  
  afterEach(() => {
    jest.useRealTimers()
  })

  const renderSignupPage = () => {
    return render(
      <BrowserRouter>
        <SignupPage />
      </BrowserRouter>
    )
  }

  it('renders signup page with title and form', () => {
    const { container } = renderSignupPage()
    
    expect(screen.getByRole('heading', { name: /sign up/i })).toBeInTheDocument()
    expect(screen.getByText(/create your account/i)).toBeInTheDocument()
    expect(container.querySelector('#FormControl-email-EmailFormGroup-email')).toBeInTheDocument()
    expect(container.querySelector('#FormControl-password-PasswordFormGroup-password')).toBeInTheDocument()
    expect(container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')).toBeInTheDocument()
    // Should NOT have username field
    expect(screen.queryByLabelText(/username/i)).not.toBeInTheDocument()
  })

  it('renders link to login page', () => {
    renderSignupPage()
    
    const loginLink = screen.getByRole('link', { name: /log in/i })
    expect(loginLink).toBeInTheDocument()
    expect(loginLink).toHaveAttribute('href', '/login')
  })


  it('successfully creates account and redirects to login', async () => {
    // Setup mocks
    authService.register.mockImplementation(() => Promise.resolve({ success: true }))
    jest.useFakeTimers()
    
    const { container } = renderSignupPage()
    
    // Get form elements
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    // Fill form
    fireEvent.change(emailInput, { target: { value: 'new@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    
    // Submit form
    fireEvent.click(submitButton)

    // Wait for the register call
    await waitFor(() => {
      expect(authService.register).toHaveBeenCalledTimes(1)
      expect(authService.register).toHaveBeenCalledWith({
        email: 'new@example.com',
        password: 'ValidPassword123!'
      })
    })

    // Check for success message
    await waitFor(() => {
      const alerts = container.querySelectorAll('.alert-success')
      const hasSuccessMessage = Array.from(alerts).some(alert => 
        alert.textContent.includes('Account created successfully')
      )
      expect(hasSuccessMessage).toBe(true)
    })
    
    // Advance timers to trigger navigation
    act(() => {
      jest.advanceTimersByTime(2000)
    })
    
    // Check navigation
    expect(mockNavigate).toHaveBeenCalledWith('/login')
    
    jest.useRealTimers()
  })

  it('displays error message when signup fails', async () => {
    // Mock failed registration
    authService.register.mockImplementation(() => Promise.reject(new Error('Email already exists')))
    
    const { container } = renderSignupPage()
    
    // Get form elements
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    // Fill form
    fireEvent.change(emailInput, { target: { value: 'existing@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    
    // Submit form
    fireEvent.click(submitButton)

    // Wait for the error to be displayed
    await waitFor(() => {
      const alerts = container.querySelectorAll('.alert-danger')
      const hasErrorMessage = Array.from(alerts).some(alert => 
        alert.textContent.toLowerCase().includes('email already exists')
      )
      expect(hasErrorMessage).toBe(true)
    })

    expect(mockNavigate).not.toHaveBeenCalled()
  })

  it('handles network errors gracefully', async () => {
    // Mock network error
    authService.register.mockImplementation(() => Promise.reject(new Error('Network error')))
    
    const { container } = renderSignupPage()
    
    // Get form elements
    const emailInput = container.querySelector('#FormControl-email-EmailFormGroup-email')
    const passwordInput = container.querySelector('#FormControl-password-PasswordFormGroup-password')
    const confirmPasswordInput = container.querySelector('#FormControl-password-PasswordFormGroup-confirmPassword')
    const submitButton = screen.getByRole('button', { name: /sign up/i })

    // Fill form
    fireEvent.change(emailInput, { target: { value: 'test@example.com' } })
    fireEvent.change(passwordInput, { target: { value: 'ValidPassword123!' } })
    fireEvent.change(confirmPasswordInput, { target: { value: 'ValidPassword123!' } })
    
    // Submit form
    fireEvent.click(submitButton)

    // Wait for the error to be displayed
    await waitFor(() => {
      const alerts = container.querySelectorAll('.alert-danger')
      const hasErrorMessage = Array.from(alerts).some(alert => 
        alert.textContent.toLowerCase().includes('network error')
      )
      expect(hasErrorMessage).toBe(true)
    })

    expect(mockNavigate).not.toHaveBeenCalled()
  })
})