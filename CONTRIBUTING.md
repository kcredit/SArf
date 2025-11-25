# Contributing to SArf

Thank you for your interest in contributing to SArf! 🎉

## Ways to Contribute

### Reporting Bugs

If you find a bug, please create an issue with:

- Clear description of the problem
- Minimal reproducible example
- Your session info (`sessionInfo()`)
- Expected vs actual behavior

### Suggesting Enhancements

Feature requests are welcome! Please:

- Check existing issues first
- Explain the use case
- Provide examples if possible

### Contributing Code

1. Fork the repository
2. Create a branch (`git checkout -b feature/AmazingFeature`)
3. Make your changes
4. Add tests if applicable
5. Run `devtools::check()` to ensure no errors
6. Commit with clear message
7. Push to your fork
8. Open a Pull Request

### Code Style

- Follow tidyverse style guide
- Use meaningful variable names
- Add comments for complex operations
- Include roxygen2 documentation for functions

### Documentation

Improvements to documentation are always appreciated:

- Fix typos or unclear explanations
- Add examples
- Improve vignettes
- Translate documentation

## Development Setup

```r
# Clone your fork
# In R:
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
```

## Questions?

Open an issue or email kevin.credit@mu.ie

Thank you for contributing! 🙏
