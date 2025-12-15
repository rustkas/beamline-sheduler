#!/bin/bash

# Beamline Constructor Coverage Configuration Script
# This script configures and runs code coverage reporting for all components

set -e

echo "=== Beamline Constructor Coverage Configuration ==="

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# C/C++ Components Coverage
setup_cpp_coverage() {
    echo "Setting up C/C++ coverage..."
    
    # C-Gateway coverage
    if [ -d "apps/c-gateway" ]; then
        echo "Building C-Gateway with coverage..."
        cd apps/c-gateway
        mkdir -p build-coverage
        cd build-coverage
        cmake -DENABLE_COVERAGE=ON -DCMAKE_BUILD_TYPE=Debug ..
        make clean && make
        cd ../..
        echo "✅ C-Gateway coverage build complete"
    fi
    
    # CAF Processor coverage (requires CAF libraries)
    if [ -d "apps/caf/processor" ]; then
        echo "Building CAF Processor with coverage..."
        cd apps/caf/processor
        mkdir -p build-coverage
        cd build-coverage
        if cmake -DENABLE_COVERAGE=ON -DCMAKE_BUILD_TYPE=Debug .. 2>/dev/null; then
            make clean && make
            echo "✅ CAF Processor coverage build complete"
        else
            echo "⚠️  CAF Processor coverage skipped (CAF libraries not available)"
        fi
        cd ../..
    fi
}

# JavaScript/TypeScript Components Coverage
setup_js_coverage() {
    echo "Setting up JavaScript/TypeScript coverage..."
    
    # Frontend coverage
    if [ -d "frontend" ]; then
        echo "Installing frontend coverage dependencies..."
        cd frontend
        npm install --save-dev @vitest/coverage-v8
        cd ..
        echo "✅ Frontend coverage setup complete"
    fi
    
    # Main tests coverage
    if [ -d "tests" ] && [ -f "tests/vitest.config.ts" ]; then
        echo "Main tests coverage already configured"
        echo "✅ Main tests coverage setup complete"
    fi
}

# Erlang Components Coverage
setup_erlang_coverage() {
    echo "Setting up Erlang coverage..."
    
    if command_exists rebar3; then
        # Router coverage
        if [ -d "apps/otp/router" ]; then
            echo "Setting up Router coverage..."
            cd apps/otp/router
            # Check if cover plugin is available
            if rebar3 help cover >/dev/null 2>&1; then
                echo "✅ Router coverage support available (use: rebar3 ct --cover)"
            else
                echo "⚠️  Router coverage requires rebar3 cover plugin"
            fi
            cd ../../..
        fi
    else
        echo "⚠️  Erlang coverage skipped (rebar3 not available)"
    fi
}

# Generate Coverage Reports
generate_reports() {
    echo "=== Generating Coverage Reports ==="
    
    # Create coverage reports directory
    mkdir -p coverage-reports
    
    # JavaScript/TypeScript coverage
    if [ -d "frontend" ]; then
        echo "Generating frontend coverage report..."
        cd frontend
        npm run test:coverage > ../coverage-reports/frontend-coverage.txt 2>&1 || true
        if [ -d "coverage" ]; then
            cp -r coverage ../coverage-reports/frontend-coverage-html
        fi
        cd ..
    fi
    
    # Main tests coverage
    if [ -d "tests" ] && [ -f "tests/vitest.config.ts" ]; then
        echo "Generating main tests coverage report..."
        cd tests
        npx vitest run --coverage > ../coverage-reports/main-tests-coverage.txt 2>&1 || true
        if [ -d "coverage" ]; then
            cp -r coverage ../coverage-reports/main-tests-coverage-html
        fi
        cd ..
    fi
    
    # C/C++ coverage
    if [ -d "apps/c-gateway/build-coverage" ]; then
        echo "Generating C-Gateway coverage report..."
        cd apps/c-gateway/build-coverage
        # Run tests if available
        if [ -f "c-gateway" ]; then
            ./c-gateway &
            PID=$!
            sleep 2
            curl -s http://localhost:8080/health > /dev/null || true
            kill $PID 2>/dev/null || true
        fi
        
        # Generate coverage report using gcov/lcov if available
        if command_exists lcov; then
            lcov --capture --directory . --output-file ../../../coverage-reports/c-gateway-coverage.info
            if command_exists genhtml; then
                genhtml ../../../coverage-reports/c-gateway-coverage.info --output-directory ../../../coverage-reports/c-gateway-coverage-html
            fi
        elif command_exists gcov; then
            echo "✅ C-Gateway coverage data available (use gcov to analyze)"
        fi
        cd ../../..
    fi
    
    echo "=== Coverage Reports Generated ==="
    echo "Reports available in: coverage-reports/"
    ls -la coverage-reports/ 2>/dev/null || echo "No reports generated yet"
}

# Main execution
main() {
    echo "Starting coverage setup..."
    
    setup_js_coverage
    setup_cpp_coverage
    setup_erlang_coverage
    
    if [ "$1" == "--generate" ]; then
        generate_reports
    fi
    
    echo "=== Coverage Setup Complete ==="
    echo ""
    echo "Usage:"
    echo "  ./coverage-config.sh           # Setup coverage for all components"
    echo "  ./coverage-config.sh --generate # Setup and generate coverage reports"
    echo ""
    echo "Component-specific usage:"
    echo "  Frontend: npm run test:coverage"
    echo "  Main tests: npx vitest run --coverage"
    echo "  C-Gateway: cd apps/c-gateway/build-coverage && make && ./c-gateway"
    echo "  Router: cd apps/otp/router && rebar3 ct --cover"
}

# Run main function with all arguments
main "$@"