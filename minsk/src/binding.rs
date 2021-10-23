use crossterm::style::ResetColor;
use std::io::stdout;

use crossterm::style::Color;
use crossterm::style::SetForegroundColor;
use crossterm::ExecutableCommand;

use crate::diagnostic::DiagnosticBag;
use crate::plumbing::Object;
use crate::plumbing::ObjectKind;
use crate::syntax::expressions::AssignmentExpressionSyntax;
use crate::syntax::expressions::BinaryExpressionSyntax;
use crate::syntax::expressions::ExpressionSyntaxRef;
use crate::syntax::expressions::LiteralExpressionSyntax;
use crate::syntax::expressions::NameExpressionSyntax;
use crate::syntax::expressions::ParenthesizedExpressionSyntax;
use crate::syntax::expressions::UnaryExpressionSyntax;
use crate::syntax::statements::BlockStatementSyntax;
use crate::syntax::statements::ExpressionStatementSyntax;
use crate::syntax::statements::ForStatementSyntax;
use crate::syntax::statements::IfStatementSyntax;
use crate::syntax::statements::StatementSyntaxRef;
use crate::syntax::statements::VariableDeclarationStatementSyntax;
use crate::syntax::statements::WhileStatementSyntax;
use crate::syntax::CompilationUnitSyntaxRef;
use crate::syntax::SyntaxKind;
use crate::syntax::SyntaxNodeRef;
use crate::text::VariableSymbol;

use self::operators::BoundBinaryOperator;
use self::operators::BoundUnaryOperator;
use self::scope::BoundGlobalScope;
use self::scope::BoundScope;

mod operators;
pub(crate) mod scope;

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundNodeKind {
    BinaryExpression,
    UnaryExpression,
    LiteralExpression,
    VariableExpression,
    AssignmentExpression,

    BlockStatement,
    ExpressionStatement,
    ForStatement,
    IfStatement,
    VariableDeclarationStatement,
    WhileStatement,
}

pub(crate) enum BoundNode {
    Expression(BoundExpression),
    Statement(BoundStatement),
}

pub(crate) enum BoundNodeRef<'a> {
    Expression(BoundExpressionRef<'a>),
    Statement(BoundStatementRef<'a>),
}

impl<'a> BoundNodeRef<'a> {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            Self::Expression(e) => e.kind(),
            Self::Statement(s) => s.kind(),
        }
    }

    fn children(&self) -> Vec<BoundNodeRef<'a>> {
        match self {
            Self::Expression(e) => e.children(),
            Self::Statement(s) => s.children(),
        }
    }

    pub(crate) fn pretty_print(&self) {
        self.pretty_print_node(&mut stdout(), String::new(), true, true);
    }

    fn pretty_print_node<W: std::io::Write>(
        &self,
        writer: &mut W,
        mut indent: String,
        is_last: bool,
        colors: bool,
    ) {
        if colors {
            writer.execute(SetForegroundColor(Color::DarkGrey)).unwrap();
        }
        write!(writer, "{}", indent).unwrap();
        let marker = if is_last {
            "└───"
        } else {
            "├───"
        };
        write!(writer, "{}", marker).unwrap();
        if colors {
            writer.execute(ResetColor).unwrap();
        }
        write!(writer, "{:?}", self.kind()).unwrap();
        indent += if is_last { "    " } else { "│   " };
        writeln!(writer).unwrap();
        let children = self.children();
        for i in 0..children.len() {
            children[i].pretty_print_node(writer, indent.clone(), i == children.len() - 1, colors);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum BoundStatement {
    Block(BoundBlockStatement),
    Expression(BoundExpressionStatement),
    For(BoundForStatement),
    If(BoundIfStatement),
    VariableDeclaration(BoundVariableDeclarationStatement),
    While(BoundWhileStatement),
}

impl BoundStatement {
    fn create_ref(&self) -> BoundStatementRef {
        match self {
            Self::Block(s) => BoundStatementRef::Block(s),
            Self::Expression(s) => BoundStatementRef::Expression(s),
            Self::For(s) => BoundStatementRef::For(s),
            Self::If(s) => BoundStatementRef::If(s),
            Self::VariableDeclaration(s) => BoundStatementRef::VariableDeclaration(s),
            Self::While(s) => BoundStatementRef::While(s),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundStatementRef<'a> {
    Block(&'a BoundBlockStatement),
    Expression(&'a BoundExpressionStatement),
    For(&'a BoundForStatement),
    If(&'a BoundIfStatement),
    VariableDeclaration(&'a BoundVariableDeclarationStatement),
    While(&'a BoundWhileStatement),
}

impl<'a> BoundStatementRef<'a> {
    fn kind(&self) -> BoundNodeKind {
        match self {
            Self::Block(_) => BoundNodeKind::BlockStatement,
            Self::Expression(_) => BoundNodeKind::ExpressionStatement,
            Self::For(_) => BoundNodeKind::ForStatement,
            Self::If(_) => BoundNodeKind::IfStatement,
            Self::VariableDeclaration(_) => BoundNodeKind::VariableDeclarationStatement,
            Self::While(_) => BoundNodeKind::WhileStatement,
        }
    }

    fn children(&self) -> Vec<BoundNodeRef<'a>> {
        match self {
            Self::Block(s) => s
                .statements
                .iter()
                .map(|s| BoundNodeRef::Statement(s.create_ref()))
                .collect::<Vec<_>>(),
            Self::Expression(s) => vec![BoundNodeRef::Expression(s.expression.create_ref())],
            Self::For(s) => vec![
                BoundNodeRef::Expression(s.lower_bound.create_ref()),
                BoundNodeRef::Expression(s.upper_bound.create_ref()),
                BoundNodeRef::Statement(s.body.create_ref()),
            ],
            Self::If(s) => {
                let mut res = vec![
                    BoundNodeRef::Expression(s.condition.create_ref()),
                    BoundNodeRef::Statement(s.then_statement.create_ref()),
                ];
                res.append(
                    &mut s
                        .else_statement
                        .as_ref()
                        .map(|s| BoundNodeRef::Statement(s.create_ref()))
                        .into_iter()
                        .collect::<Vec<_>>(),
                );
                res
            }
            Self::VariableDeclaration(s) => {
                vec![BoundNodeRef::Expression(s.initializer.create_ref())]
            }
            Self::While(s) => vec![
                BoundNodeRef::Expression(s.condition.create_ref()),
                BoundNodeRef::Statement(s.body.create_ref()),
            ],
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BoundBlockStatement {
    pub(crate) statements: Vec<BoundStatement>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundExpressionStatement {
    pub(crate) expression: BoundExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundForStatement {
    pub(crate) variable: VariableSymbol,
    pub(crate) lower_bound: BoundExpression,
    pub(crate) upper_bound: BoundExpression,
    pub(crate) body: Box<BoundStatement>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundIfStatement {
    pub(crate) condition: BoundExpression,
    pub(crate) then_statement: Box<BoundStatement>,
    pub(crate) else_statement: Option<Box<BoundStatement>>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundVariableDeclarationStatement {
    pub(crate) variable: VariableSymbol,
    pub(crate) initializer: BoundExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundWhileStatement {
    pub(crate) condition: BoundExpression,
    pub(crate) body: Box<BoundStatement>,
}

#[derive(Debug, Clone)]
pub(crate) enum BoundExpression {
    Binary(BoundBinaryExpression),
    Unary(BoundUnaryExpression),
    Literal(BoundLiteralExpression),
    Variable(BoundVariableExpression),
    Assignment(BoundAssignmentExpression),
}

impl BoundExpression {
    fn create_ref(&self) -> BoundExpressionRef {
        match self {
            Self::Binary(e) => BoundExpressionRef::Binary(e),
            Self::Unary(e) => BoundExpressionRef::Unary(e),
            Self::Literal(e) => BoundExpressionRef::Literal(e),
            Self::Variable(e) => BoundExpressionRef::Variable(e),
            Self::Assignment(e) => BoundExpressionRef::Assignment(e),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundExpressionRef<'a> {
    Binary(&'a BoundBinaryExpression),
    Unary(&'a BoundUnaryExpression),
    Literal(&'a BoundLiteralExpression),
    Variable(&'a BoundVariableExpression),
    Assignment(&'a BoundAssignmentExpression),
}

impl<'a> BoundExpressionRef<'a> {
    pub(crate) fn kind(&self) -> BoundNodeKind {
        match self {
            Self::Binary(_) => BoundNodeKind::BinaryExpression,
            Self::Unary(_) => BoundNodeKind::UnaryExpression,
            Self::Literal(_) => BoundNodeKind::LiteralExpression,
            Self::Variable(_) => BoundNodeKind::VariableExpression,
            Self::Assignment(_) => BoundNodeKind::AssignmentExpression,
        }
    }

    pub(crate) fn get_type(&self) -> ObjectKind {
        match self {
            Self::Binary(e) => e.operator.result_type,
            Self::Unary(e) => e.operator.result_type,
            Self::Literal(e) => e.value.kind(),
            Self::Variable(e) => e.variable.kind,
            Self::Assignment(e) => e.expression.create_ref().get_type(),
        }
    }

    fn children(&self) -> Vec<BoundNodeRef<'a>> {
        match self {
            Self::Binary(e) => vec![
                BoundNodeRef::Expression(e.left.create_ref()),
                BoundNodeRef::Expression(e.right.create_ref()),
            ],
            Self::Unary(e) => vec![BoundNodeRef::Expression(e.operand.create_ref())],
            Self::Literal(_) => Vec::new(),
            Self::Variable(_) => Vec::new(),
            Self::Assignment(e) => vec![BoundNodeRef::Expression(e.expression.create_ref())],
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundBinaryOperatorKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equality,
    Inequality,
    Less,
    LessOrEquals,
    Greater,
    GreaterOrEquals,
}

#[derive(Debug, Clone)]
pub struct BoundBinaryExpression {
    pub(crate) left: Box<BoundExpression>,
    pub(crate) operator: &'static BoundBinaryOperator,
    pub(crate) right: Box<BoundExpression>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundUnaryOperatorKind {
    Identity,
    Negation,
    LogicalNegation,
    BitwiseNegation,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundUnaryExpression {
    pub(crate) operator: &'static BoundUnaryOperator,
    pub(crate) operand: Box<BoundExpression>,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundLiteralExpression {
    pub(crate) value: Object,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundVariableExpression {
    pub(crate) variable: VariableSymbol,
}

#[derive(Debug, Clone)]
pub(crate) struct BoundAssignmentExpression {
    pub(crate) variable: VariableSymbol,
    pub(crate) expression: Box<BoundExpression>,
}

pub(crate) struct Binder {
    pub(crate) diagnostics: DiagnosticBag,
    scope: BoundScope,
}

impl Binder {
    fn bind_assignment_expression(
        &mut self,
        e: &AssignmentExpressionSyntax,
    ) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();
        let expression = self.bind_expression(e.expression.create_ref());

        let variable = if let Some(variable) = self.scope.try_lookup(&name) {
            variable.clone()
        } else {
            self.diagnostics
                .report_undefined_name(e.identifier_token.span(), &name);
            return expression;
        };

        if variable.is_read_only {
            self.diagnostics
                .report_cannot_assign(e.equals_token.span(), &name);
        }

        if expression.create_ref().get_type() != variable.kind {
            self.diagnostics.report_cannot_convert(
                SyntaxNodeRef::Expression(e.expression.create_ref()).span(),
                expression.create_ref().get_type(),
                variable.kind,
            );
            return expression;
        }

        Box::new(BoundExpression::Assignment(BoundAssignmentExpression {
            variable,
            expression,
        }))
    }

    fn bind_binary_expression(&mut self, e: &BinaryExpressionSyntax) -> Box<BoundExpression> {
        let left = self.bind_expression(e.left.create_ref());
        let right = self.bind_expression(e.right.create_ref());
        let operator = BoundBinaryOperator::bind(
            e.operator_token.kind,
            left.create_ref().get_type(),
            right.create_ref().get_type(),
        );
        if let Some(operator) = operator {
            Box::new(BoundExpression::Binary(BoundBinaryExpression {
                left,
                operator,
                right,
            }))
        } else {
            self.diagnostics.report_undefined_binary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                left.create_ref().get_type(),
                right.create_ref().get_type(),
            );
            left
        }
    }

    fn bind_block_statement(&mut self, s: &BlockStatementSyntax) -> Box<BoundStatement> {
        let mut statements = Vec::new();

        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, &mut self.scope);
        self.scope = BoundScope::new(Some(Box::new(scope)));

        for statement_syntax in &s.statements {
            let statement = self.bind_statement(statement_syntax.create_ref());
            statements.push(*statement);
        }

        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, self.scope.parent.as_mut().unwrap());
        self.scope = scope;

        Box::new(BoundStatement::Block(BoundBlockStatement { statements }))
    }

    pub(crate) fn bind_expression(
        &mut self,
        expression: ExpressionSyntaxRef,
    ) -> Box<BoundExpression> {
        match expression {
            ExpressionSyntaxRef::Binary(e) => self.bind_binary_expression(e),
            ExpressionSyntaxRef::Unary(e) => self.bind_unary_expression(e),
            ExpressionSyntaxRef::Literal(e) => self.bind_literal_expression(e),
            ExpressionSyntaxRef::Parenthesized(e) => self.bind_parenthesized_expression(e),
            ExpressionSyntaxRef::Name(e) => self.bind_name_expression(e),
            ExpressionSyntaxRef::Assignment(e) => self.bind_assignment_expression(e),
        }
    }

    fn bind_expression_statement(&mut self, s: &ExpressionStatementSyntax) -> Box<BoundStatement> {
        let expression = self.bind_expression(s.expression.create_ref());
        Box::new(BoundStatement::Expression(BoundExpressionStatement {
            expression: *expression,
        }))
    }

    fn bind_expression_with_type(
        &mut self,
        expression: ExpressionSyntaxRef,
        target_type: ObjectKind,
    ) -> Box<BoundExpression> {
        let result = self.bind_expression(expression);
        if result.create_ref().get_type() != target_type {
            self.diagnostics.report_cannot_convert(
                SyntaxNodeRef::Expression(expression).span(),
                result.create_ref().get_type(),
                target_type,
            );
        }
        result
    }

    fn bind_for_statement(&mut self, s: &ForStatementSyntax) -> Box<BoundStatement> {
        let lower_bound =
            *self.bind_expression_with_type(s.lower_bound.create_ref(), ObjectKind::Number);
        let upper_bound =
            *self.bind_expression_with_type(s.upper_bound.create_ref(), ObjectKind::Number);
        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, &mut self.scope);
        self.scope = BoundScope::new(Some(Box::new(scope)));
        let name = s.identifier_token.text.clone();
        let variable = VariableSymbol {
            name,
            is_read_only: false,
            kind: ObjectKind::Number,
        };
        // will always succeed because it's a new scope
        self.scope.try_declare(variable.clone());
        let body = self.bind_statement(s.body.create_ref());
        let mut scope = BoundScope::new(None);
        std::mem::swap(&mut scope, &mut self.scope.parent.as_mut().unwrap());
        self.scope = scope;
        Box::new(BoundStatement::For(BoundForStatement {
            variable,
            lower_bound,
            upper_bound,
            body,
        }))
    }

    pub(crate) fn bind_global_scope(
        previous: Option<&BoundGlobalScope>,
        syntax: CompilationUnitSyntaxRef,
    ) -> BoundGlobalScope {
        let parent_scope = Self::create_parent_scopes(previous);
        let mut binder = Binder::new(parent_scope);
        let statement = binder.bind_statement(syntax.statement);
        let variables = binder
            .scope
            .get_declared_variables()
            .into_iter()
            .cloned()
            .collect();
        let diagnostics = binder.diagnostics.into_iter().collect::<Vec<_>>();
        BoundGlobalScope {
            previous: None,
            diagnostics,
            variables,
            statement: *statement,
        }
    }

    fn bind_if_statement(&mut self, s: &IfStatementSyntax) -> Box<BoundStatement> {
        let condition =
            self.bind_expression_with_type(s.condition.create_ref(), ObjectKind::Boolean);
        let then_statement = self.bind_statement(s.then_statement.create_ref());
        let else_statement = s
            .else_clause
            .as_ref()
            .map(|c| self.bind_statement(c.else_statement.create_ref()));
        Box::new(BoundStatement::If(BoundIfStatement {
            condition: *condition,
            then_statement,
            else_statement,
        }))
    }

    fn bind_literal_expression(&self, e: &LiteralExpressionSyntax) -> Box<BoundExpression> {
        Box::new(BoundExpression::Literal(BoundLiteralExpression {
            value: e.value.clone(),
        }))
    }

    fn bind_name_expression(&mut self, e: &NameExpressionSyntax) -> Box<BoundExpression> {
        let name = e.identifier_token.text.clone();

        let variable = self.scope.try_lookup(&name);

        match variable {
            Some(v) => Box::new(BoundExpression::Variable(BoundVariableExpression {
                variable: v.clone(),
            })),
            None => {
                self.diagnostics
                    .report_undefined_name(e.identifier_token.span(), &name);
                Box::new(BoundExpression::Literal(BoundLiteralExpression {
                    value: Object::Number(0),
                }))
            }
        }
    }

    fn bind_parenthesized_expression(
        &mut self,
        e: &ParenthesizedExpressionSyntax,
    ) -> Box<BoundExpression> {
        self.bind_expression(e.expression.create_ref())
    }

    pub(crate) fn bind_statement(&mut self, statement: StatementSyntaxRef) -> Box<BoundStatement> {
        match statement {
            StatementSyntaxRef::Block(s) => self.bind_block_statement(s),
            StatementSyntaxRef::Expression(s) => self.bind_expression_statement(s),
            StatementSyntaxRef::For(s) => self.bind_for_statement(s),
            StatementSyntaxRef::If(s) => self.bind_if_statement(s),
            StatementSyntaxRef::VariableDeclaration(s) => {
                self.bind_variable_declaration_statement(s)
            }
            StatementSyntaxRef::While(s) => self.bind_while_statement(s),
        }
    }

    fn bind_unary_expression(&mut self, e: &UnaryExpressionSyntax) -> Box<BoundExpression> {
        let operand = self.bind_expression(e.operand.create_ref());
        let operator =
            BoundUnaryOperator::bind(e.operator_token.kind, operand.create_ref().get_type());
        if let Some(operator) = operator {
            Box::new(BoundExpression::Unary(BoundUnaryExpression {
                operator,
                operand,
            }))
        } else {
            self.diagnostics.report_undefined_unary_operator(
                e.operator_token.span(),
                e.operator_token.text.clone(),
                operand.create_ref().get_type(),
            );
            operand
        }
    }

    fn bind_variable_declaration_statement(
        &mut self,
        s: &VariableDeclarationStatementSyntax,
    ) -> Box<BoundStatement> {
        let name = s.identifier.text.clone();
        let initializer = self.bind_expression(s.initializer.create_ref());
        let is_read_only = s.keyword.kind == SyntaxKind::LetKeyword;
        let variable = VariableSymbol {
            name: name.clone(),
            is_read_only,
            kind: initializer.create_ref().get_type(),
        };
        if !self.scope.try_declare(variable.clone()) {
            self.diagnostics
                .report_variable_already_declared(s.identifier.span(), &name);
        }
        Box::new(BoundStatement::VariableDeclaration(
            BoundVariableDeclarationStatement {
                variable,
                initializer: *initializer,
            },
        ))
    }

    fn bind_while_statement(&mut self, s: &WhileStatementSyntax) -> Box<BoundStatement> {
        let condition =
            self.bind_expression_with_type(s.condition.create_ref(), ObjectKind::Boolean);
        let body = self.bind_statement(s.body.create_ref());
        Box::new(BoundStatement::While(BoundWhileStatement {
            condition: *condition,
            body,
        }))
    }

    fn create_parent_scopes(mut previous: Option<&BoundGlobalScope>) -> Option<Box<BoundScope>> {
        let mut stack = Vec::new();
        while let Some(p) = previous {
            stack.push(p);
            previous = p.previous.as_ref().map(|p| p.as_ref());
        }

        let mut parent = None;
        while let Some(global) = stack.pop() {
            let mut scope = BoundScope::new(parent);
            for v in &global.variables {
                scope.try_declare(v.clone());
            }
            parent = Some(Box::new(scope));
        }
        parent
    }

    pub(crate) fn new(scope: Option<Box<BoundScope>>) -> Self {
        Self {
            diagnostics: DiagnosticBag::new(),
            scope: BoundScope::new(scope),
        }
    }
}
