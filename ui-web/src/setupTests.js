/**
 * Created by JimBarrows on 2019-01-20.
 */
import '@testing-library/jest-dom'

// Mock FontAwesome to avoid dependency issues in tests
jest.mock('@fortawesome/fontawesome-svg-core', () => ({
  library: {
    add: jest.fn()
  }
}));

jest.mock('@fortawesome/free-solid-svg-icons', () => ({
  faBan: 'faBan',
  faEdit: 'faEdit',
  faPlus: 'faPlus',
  faSave: 'faSave',
  faSync: 'faSync',
  faTrash: 'faTrash'
}));

jest.mock('@fortawesome/react-fontawesome', () => ({
  FontAwesomeIcon: () => null
}));
