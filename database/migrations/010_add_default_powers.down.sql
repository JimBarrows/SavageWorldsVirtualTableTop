-- Remove the trigger
DROP TRIGGER IF EXISTS add_default_powers_on_insert ON plot_points;

-- Remove the function
DROP FUNCTION IF EXISTS add_default_powers_to_plot_point();

-- Note: We don't remove the default powers from existing plot points as that would be data loss